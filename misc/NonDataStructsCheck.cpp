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
// these are from misc-non-private-member-variables-in-classes
AST_MATCHER(CXXRecordDecl, hasMethods) {
  // doesn't count template methods until their instantiation
  return std::distance(Node.method_begin(), Node.method_end()) != 0;
}

AST_MATCHER(CXXRecordDecl, hasNonPublicMemberVariable) {
  return cxxRecordDecl(has(fieldDecl(unless(isPublic()))))
      .matches(Node, Finder, Builder);
}
} // namespace

void NonDataStructsCheck::registerMatchers(MatchFinder *Finder) {
  Finder->addMatcher(
      cxxRecordDecl(isStruct(),
                    anyOf(hasMethods(), hasNonPublicMemberVariable()))
          .bind("record"),
      this);
}

void NonDataStructsCheck::check(const MatchFinder::MatchResult &Result) {

  const auto *MatchedRecord = Result.Nodes.getNodeAs<CXXRecordDecl>("record");
  assert(MatchedRecord);
  
  diag(MatchedRecord->getLocation(),
       "struct %0 has methods or non-public data members")
      << MatchedRecord;
}

} // namespace misc
} // namespace tidy
} // namespace clang
