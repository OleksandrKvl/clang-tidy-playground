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

AST_MATCHER_P(CXXRecordDecl, hasNonPublicBase,
              ast_matchers::internal::Matcher<CXXRecordDecl>, InnerMatcher) {
  if (!Node.hasDefinition()) {
    return false;
  }

  for (const auto &BaseSpec : Node.bases()) {
    if (BaseSpec.getAccessSpecifier() != AS_public) {
      const auto Record = BaseSpec.getType()->getAsCXXRecordDecl();
      if (Record && InnerMatcher.matches(*Record, Finder, Builder)) {
        return true;
      }
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

AST_MATCHER_P(CXXConstructorDecl, shouldAllowCtor,
              NonDataStructsCheck::AllowedCtorKind, AllowedCtors) {
  if (Node.isCopyOrMoveConstructor() ||
      (AllowedCtors == NonDataStructsCheck::AllowedCtorKind::None)) {
    return false;
  } else if (AllowedCtors == NonDataStructsCheck::AllowedCtorKind::Primary) {
    return true;
  } else {
    return Node.isDefaultConstructor();
  }
}
} // namespace

NonDataStructsCheck::AllowedCtorKind
NonDataStructsCheck::StringToCtorKind(StringRef Str) {
  if (Str == "default") {
    return AllowedCtorKind::Default;
  } else if (Str == "primary") {
    return AllowedCtorKind::Primary;
  }

  assert((Str == "none") && "Bad AllowedCtors string");
  return AllowedCtorKind::None;
}

std::string NonDataStructsCheck::CtorKindToString(const AllowedCtorKind Kind) {
  switch (Kind) {
  case AllowedCtorKind::None:
    return "none";
  case AllowedCtorKind::Default:
    return "default";
  case AllowedCtorKind::Primary:
    return "primary";
  default:
    llvm_unreachable("Bad AllowedCtorKind value");
  }
}

NonDataStructsCheck::NonDataStructsCheck(StringRef Name,
                                         ClangTidyContext *Context)
    : ClangTidyCheck(Name, Context), AllowNonEmptyCtorBody{Options.get(
                                         "AllowNonEmptyCtorBody", false)},
      AllowDefaultMemberInit{Options.get("AllowDefaultMemberInit", false)},
      AllowedCtors{StringToCtorKind(Options.get(
          "AllowedCtors", CtorKindToString(AllowedCtorKind::None)))},
      SkipStateless{Options.get("SkipStateless", true)} {}

void NonDataStructsCheck::storeOptions(ClangTidyOptions::OptionMap &Opts) {
  Options.store(Opts, "AllowNonEmptyCtorBody", AllowNonEmptyCtorBody);
  Options.store(Opts, "AllowDefaultMemberInit", AllowDefaultMemberInit);
  Options.store(Opts, "AllowedCtors", CtorKindToString(AllowedCtors));
  Options.store(Opts, "SkipStateless", SkipStateless);
}

void NonDataStructsCheck::registerMatchers(MatchFinder *Finder) {
  if (!getLangOpts().CPlusPlus) {
    return;
  }

  const auto ShouldAllowNonEmptyCtorBody =
      anyOf(boolean(AllowNonEmptyCtorBody), hasTrivialBody());

  const auto ShouldAllowDefaultMemberInit =
      anyOf(boolean(AllowDefaultMemberInit), unless(has(initListExpr())));

  Finder->addMatcher(
      cxxRecordDecl(
          isStruct(), anyOf(boolean(!SkipStateless), has(fieldDecl())),
          anyOf(
              has(cxxMethodDecl(isUserProvided(),
                                unless(anyOf(isStaticStorageClass(),
                                             cxxConstructorDecl(
                                                 shouldAllowCtor(AllowedCtors),
                                                 ShouldAllowNonEmptyCtorBody))))
                      .bind("method")),
              has(fieldDecl(
                      unless(allOf(isPublic(), ShouldAllowDefaultMemberInit)))
                      .bind("field")),
              anyOf(hasNonPublicBase(cxxRecordDecl().bind("np_base")),
                    hasDirectBase(
                        cxxRecordDecl(unless(isStruct())).bind("ns_base")))))
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
         "struct %0 has non-public or default-initialized data member %1")
        << MatchedRecord << MatchedField;
  } else if (const auto MatchedNonStructBase =
                 Result.Nodes.getNodeAs<CXXRecordDecl>("ns_base")) {
    diag(MatchedRecord->getLocation(), "struct %0 has non-struct base %1")
        << MatchedRecord << MatchedNonStructBase;
  } else if (const auto MatchedNonPublicBase =
                 Result.Nodes.getNodeAs<CXXRecordDecl>("np_base")) {
    diag(MatchedRecord->getLocation(), "struct %0 has non-public base %1")
        << MatchedRecord << MatchedNonPublicBase;
  }
}
} // namespace misc
} // namespace tidy
} // namespace clang
