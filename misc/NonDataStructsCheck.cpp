//===--- NonDataStructsCheck.cpp - clang-tidy -----------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "NonDataStructsCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace misc {
namespace {
AST_MATCHER_P(CXXRecordDecl, hasDirectBase,
              ast_matchers::internal::Matcher<CXXRecordDecl>, InnerMatcher) {
  if (!Node.hasDefinition()) {
    return false;
  }

  for (const auto &BaseSpec : Node.bases()) {
    const auto Record = BaseSpec.getType()->getAsCXXRecordDecl();
    if (Record && InnerMatcher.matches(*Record, Finder, Builder)) {
      return true;
    }
  }
  return false;
}

AST_MATCHER(CXXConstructorDecl, hasBitFieldInitializersOnly) {
  if (Node.inits().empty()) {
    return false;
  }

  for (const auto &initializer : Node.inits()) {
    const auto isDirectBitFieldInitializer =
        initializer->isMemberInitializer() &&
        initializer->getMember()->isBitField();

    if (!initializer->isInClassMemberInitializer() &&
        !isDirectBitFieldInitializer) {
      return false;
    }
  }

  return true;
}

AST_MATCHER(CXXRecordDecl, hasNonPublicBase) {
  if (!Node.hasDefinition()) {
    return false;
  }

  for (const auto &BaseSpec : Node.bases()) {
    if (BaseSpec.getAccessSpecifier() != AS_public) {
      return true;
    }
  }

  return false;
}

// from /misc/NonPrivateMemberVariablesInClassesCheck.cpp
AST_POLYMORPHIC_MATCHER_P(boolean, AST_POLYMORPHIC_SUPPORTED_TYPES(Stmt, Decl),
                          bool, Boolean) {
  return Boolean;
}

// from /readability/ConvertMemberFunctionsToStatic.cpp
AST_MATCHER(CXXMethodDecl, hasTrivialBody) { return Node.hasTrivialBody(); }
} // namespace

NonDataStructsCheck::NonDataStructsCheck(StringRef Name,
                                         ClangTidyContext *Context)
    : ClangTidyCheck(Name, Context),
      AllowConstructors(Options.get("AllowConstructors", false)),
      SkipStateless{Options.get("SkipStateless", true)} {}

void NonDataStructsCheck::storeOptions(ClangTidyOptions::OptionMap &Opts) {
  Options.store(Opts, "AllowConstructors", AllowConstructors);
  Options.store(Opts, "SkipStateless", SkipStateless);
}

void NonDataStructsCheck::registerMatchers(MatchFinder *Finder) {
  if (!getLangOpts().CPlusPlus) {
    return;
  }

  auto ShouldAllowCtor =
      allOf(boolean(AllowConstructors), cxxConstructorDecl());

  // since C++20 default member initialization for bit-fields is supported
  auto IsBitFieldInitCtor = allOf(
      boolean(!getLangOpts().CPlusPlus2a),
      cxxConstructorDecl(hasTrivialBody(), hasBitFieldInitializersOnly()));

  Finder->addMatcher(
      cxxRecordDecl(
          isStruct(), anyOf(boolean(!SkipStateless), has(fieldDecl())),
          anyOf(has(cxxMethodDecl(
                        isUserProvided(),
                        unless(anyOf(isStaticStorageClass(), ShouldAllowCtor,
                                     IsBitFieldInitCtor)))
                        .bind("method")),
                has(fieldDecl(unless(isPublic())).bind("field")),
                anyOf(hasNonPublicBase(),
                      hasDirectBase(
                          cxxRecordDecl(unless(isStruct())).bind("base")))))
          .bind("record"),
      this);
}

void NonDataStructsCheck::check(const MatchFinder::MatchResult &Result) {

  const auto MatchedRecord = Result.Nodes.getNodeAs<CXXRecordDecl>("record");
  assert(MatchedRecord);

  if (const auto MatchedMethod =
          Result.Nodes.getNodeAs<CXXMethodDecl>("method")) {
    diag(MatchedRecord->getLocation(), "struct %0 has non-static method %1")
        << MatchedRecord << MatchedMethod;
  } else if (const auto MatchedField =
                 Result.Nodes.getNodeAs<FieldDecl>("field")) {
    diag(MatchedRecord->getLocation(),
         "struct %0 has non-public data member %1")
        << MatchedRecord << MatchedField;
  } else if (const auto MatchedBase =
                 Result.Nodes.getNodeAs<CXXRecordDecl>("base")) {
    diag(MatchedRecord->getLocation(), "struct %0 has non-struct base %1")
        << MatchedRecord << MatchedBase;
  } else {
    diag(MatchedRecord->getLocation(), "struct %0 has non-public base")
        << MatchedRecord;
  }
}

} // namespace misc
} // namespace tidy
} // namespace clang
