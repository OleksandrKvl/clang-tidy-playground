//===--- EnforceThisStyleCheck.cpp - clang-tidy -------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "EnforceThisStyleCheck.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"

using namespace clang::ast_matchers;

namespace clang {
namespace tidy {
namespace misc {

EnforceThisStyleCheck::EnforceThisStyleCheck(StringRef Name, ClangTidyContext *Context)
    : ClangTidyCheck(Name, Context) {
  const auto StyleStr = Options.get("Style", "implicit");
  if (StyleStr == "explicit") {
    Style = ThisStyle::Explicit;
  }
}
// void EnforceThisStyleCheck::storeOptions(ClangTidyOptions::OptionMap &Opts) {
//   Options.store(Opts, "Style", "implicit");
//   // Options.store(); ???
// }

void EnforceThisStyleCheck::registerMatchers(MatchFinder *Finder) {
  if (!getLangOpts().CPlusPlus)
    return;

  Finder->addMatcher(
      cxxMethodDecl(
          isDefinition(),
          forEachDescendant(memberExpr(has(cxxThisExpr().bind("thisExpr")))
                                .bind("memberExpr")))
          .bind("method"),
      this);
}

// Rename the whole module to "EnforceThisStyle"
bool hasVariableWithName(const CXXMethodDecl &Function, ASTContext &Context,
                         StringRef Name) {
  const auto Matches =
      match(decl(hasDescendant(varDecl(hasName(Name)))), Context);
  return !Matches.empty();
}

void EnforceThisStyleCheck::removeExplicitThis(const SourceManager &SM,
                                           const MemberExpr &MembExpr) {
  const auto ThisStart = MembExpr.getBeginLoc();
  const auto ThisEnd = MembExpr.getMemberLoc();
  const auto ThisRange = Lexer::makeFileCharRange(
      CharSourceRange::getCharRange(ThisStart, ThisEnd), SM, getLangOpts());

  diag(MembExpr.getBeginLoc(), "remove 'this->'", DiagnosticIDs::Note)
      << FixItHint::CreateRemoval(ThisRange);
}

void EnforceThisStyleCheck::addExplicitThis(const CXXThisExpr &ThisExpr) {
  diag(ThisExpr.getLocation(), "insert 'this->'", DiagnosticIDs::Note)
      << FixItHint::CreateInsertion(ThisExpr.getLocation(), "this->");
}

// checks whether the expression relates to a special member function
static bool isSimpleMember(const MemberExpr &MembExpr) {
  const auto MemberDecl = MembExpr.getMemberDecl();
  assert(MemberDecl);

  const auto MethodDecl = dyn_cast<CXXMethodDecl>(MemberDecl);
  return !MethodDecl || MethodDecl->getIdentifier();
}

void EnforceThisStyleCheck::check(const MatchFinder::MatchResult &Result) {
  const auto MatchedThis = Result.Nodes.getNodeAs<CXXThisExpr>("thisExpr");
  assert(MatchedThis);

  const auto ThisLocation = MatchedThis->getLocation();
  if (ThisLocation.isInvalid() || ThisLocation.isMacroID())
    return;

  const auto MatchedMember = Result.Nodes.getNodeAs<MemberExpr>("memberExpr");
  assert(MatchedMember);

  if ((Style == ThisStyle::Implicit) && !MatchedThis->isImplicit()) {
    const auto MatchedMethod = Result.Nodes.getNodeAs<CXXMethodDecl>("method");
    assert(MatchedMethod);

    if (isSimpleMember(*MatchedMember)) {
      if (!hasVariableWithName(*MatchedMethod, *Result.Context,
                               MatchedMember->getMemberDecl()->getName())) {
        diag(MatchedThis->getLocation(), "explicit `this->` detected");
        removeExplicitThis(*Result.SourceManager, *MatchedMember);
      }
    }
  } else if ((Style == ThisStyle::Explicit) && MatchedThis->isImplicit()) {
    if (!MatchedMember->hasQualifier()) {
      diag(MatchedThis->getLocation(), "implicit `this->` detected");
      addExplicitThis(*MatchedThis);
    }
  }
}
} // namespace misc
} // namespace tidy
} // namespace clang
