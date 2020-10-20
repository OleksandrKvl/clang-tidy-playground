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
void NonDataStructsCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(
      cxxRecordDecl(
          isStruct(),
          anyOf(has(cxxMethodDecl(
                        unless(anyOf(isImplicit(), isDefaulted(), isDeleted(),
                                     isStaticStorageClass())))
                        .bind("method")),
                has(fieldDecl(unless(isPublic())).bind("field"))))
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
  } else {
    const auto MatchedField = Result.Nodes.getNodeAs<FieldDecl>("field");
    assert(MatchedField);

    diag(MatchedRecord->getLocation(),
         "struct %0 has non-public data member %1")
        << MatchedRecord << MatchedField;
  }
}

} // namespace misc
} // namespace tidy
} // namespace clang
