//===--- ExplicitThisCheck.cpp - clang-tidy -------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "ExplicitThisCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace misc {

void ExplicitThisCheck::registerMatchers(MatchFinder *Finder) {
  // FIXME: support configuration
  Finder->addMatcher(cxxThisExpr().bind("expr"), this);
}

void ExplicitThisCheck::check(const MatchFinder::MatchResult &Result) {
  const auto MatchedThis = Result.Nodes.getNodeAs<CXXThisExpr>("expr");
  if (MatchedThis->isImplicit()) {
    diag(MatchedThis->getLocation(), "implicit `this` expression detected");
    diag(MatchedThis->getLocation(), "insert 'this->'", DiagnosticIDs::Note)
        << FixItHint::CreateInsertion(MatchedThis->getLocation(), "this->");
  }
}

} // namespace misc
} // namespace tidy
} // namespace clang
