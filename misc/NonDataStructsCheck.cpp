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
} // namespace

void NonDataStructsCheck::registerMatchers(MatchFinder *Finder) {
  if (!getLangOpts().CPlusPlus) {
    return;
  }

  Finder->addMatcher(
      cxxRecordDecl(
          isStruct(),
          anyOf(has(cxxMethodDecl(isUserProvided(),
                                  unless(isStaticStorageClass()))
                        .bind("method")),
                has(fieldDecl(unless(isPublic())).bind("field")),
                hasDirectBase(cxxRecordDecl(unless(isStruct())).bind("base"))))
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
  } else {
    const auto MatchedBase = Result.Nodes.getNodeAs<CXXRecordDecl>("base");
    assert(MatchedBase);

    diag(MatchedRecord->getLocation(), "struct %0 has non-struct base %1")
        << MatchedRecord << MatchedBase;
  }
}

} // namespace misc
} // namespace tidy
} // namespace clang
