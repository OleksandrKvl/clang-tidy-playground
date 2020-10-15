//===--- EnforceThisStyleCheck.cpp - clang-tidy
//-------------------------------===//
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
EnforceThisStyleCheck::EnforceThisStyleCheck(StringRef Name,
                                             ClangTidyContext *Context)
    : ClangTidyCheck(Name, Context) {
  const auto StyleStr = Options.get("Style", "implicit");
  if (StyleStr == "explicit") {
    Style = ThisStyle::Explicit;
  }
}

void EnforceThisStyleCheck::storeOptions(ClangTidyOptions::OptionMap &Opts) {
  if (Style == ThisStyle::Implicit) {
    Options.store(Opts, "Style", "implicit");
  } else {
    Options.store(Opts, "Style", "explicit");
  }
}

static bool isTemplateInstantiation(const CXXRecordDecl &Class) {
  // Do like isTemplateInstantiation() matcher does. We don't care about
  // explicit specializations because they don't have template arguments, which
  // means they can't have type-dependent names.
  const auto SpecKind = Class.getTemplateSpecializationKind();
  const auto IsInstantiated =
      (SpecKind == TSK_ImplicitInstantiation ||
       SpecKind == TSK_ExplicitInstantiationDefinition ||
       SpecKind == TSK_ExplicitInstantiationDeclaration);

  return IsInstantiated;
}

void EnforceThisStyleCheck::registerMatchers(MatchFinder *Finder) {
  if (!getLangOpts().CPlusPlus) {
    return;
  }

  Finder->addMatcher(
      cxxMethodDecl(
          // don't check compiler-generated function bodies
          isDefinition(), unless(anyOf(isImplicit(), isDefaulted())),
          forEachDescendant(
              memberExpr(has(ignoringImpCasts(cxxThisExpr().bind("thisExpr"))))
                  .bind("memberExpr")))
          .bind("methodDecl"),
      this);
}

static bool hasVariableWithName(const CXXMethodDecl &Function,
                                ASTContext &Context, const StringRef Name) {
  const auto Matches =
      match(decl(hasDescendant(varDecl(hasName(Name)))), Function, Context);

  return !Matches.empty();
}

static bool hasDirectMember(const CXXRecordDecl &Record, ASTContext &Context,
                            const StringRef Name) {
  const auto Matches =
      match(cxxRecordDecl(has(namedDecl(hasName(Name)))), Record, Context);

  return !Matches.empty();
}

static bool isDependentName(const CXXMethodDecl &Method,
                            const MemberExpr &MembExpr, ASTContext &Context) {
  const auto Class = Method.getParent();
  assert(Class);

  return isTemplateInstantiation(*Class) &&
         !hasDirectMember(*Class, Context, MembExpr.getMemberDecl()->getName());
}

void EnforceThisStyleCheck::removeExplicitThis(const SourceLocation ThisStart,
                                               const SourceLocation ThisEnd,
                                               const SourceManager &SM) {
  const auto ThisRange = Lexer::makeFileCharRange(
      CharSourceRange::getCharRange(ThisStart, ThisEnd), SM, getLangOpts());

  diag(ThisStart, "remove 'this->'") << FixItHint::CreateRemoval(ThisRange);
}

void EnforceThisStyleCheck::removeExplicitThis(const SourceManager &SM,
                                               const MemberExpr &MembExpr) {
  const auto ThisStart = MembExpr.getBeginLoc();
  auto ThisEnd = MembExpr.getMemberLoc();
  if (MembExpr.hasQualifier()) {
    ThisEnd = MembExpr.getQualifierLoc().getBeginLoc();
  }

  removeExplicitThis(ThisStart, ThisEnd, SM);
}

void EnforceThisStyleCheck::addExplicitThis(const CXXThisExpr &ThisExpr) {
  diag(ThisExpr.getBeginLoc(), "insert 'this->'")
      << FixItHint::CreateInsertion(ThisExpr.getBeginLoc(), "this->");
}

static bool isNonSpecialMember(const MemberExpr &MembExpr) {
  const auto MemberDecl = MembExpr.getMemberDecl();
  assert(MemberDecl);

  const auto MethodDecl = dyn_cast<CXXMethodDecl>(MemberDecl);
  return !MethodDecl || MethodDecl->getIdentifier();
}

void EnforceThisStyleCheck::check(const MatchFinder::MatchResult &Result) {
  const auto ThisExpr = Result.Nodes.getNodeAs<CXXThisExpr>("thisExpr");
  assert(ThisExpr);

  const auto ThisLocation = ThisExpr->getLocation();

  if (ThisLocation.isInvalid() ||
      (ThisLocation.isMacroID() &&
       (Lexer::getImmediateMacroName(ThisLocation, *Result.SourceManager,
                                     getLangOpts()) != "assert"))) {
    return;
  }

  if ((Style == ThisStyle::Implicit) && !ThisExpr->isImplicit()) {
    const auto MembExpr = Result.Nodes.getNodeAs<MemberExpr>("memberExpr");
    assert(MembExpr);
    const auto MethodDecl = Result.Nodes.getNodeAs<CXXMethodDecl>("methodDecl");
    assert(MethodDecl);
    const auto Class = MethodDecl->getParent();
    assert(Class);

    if (isNonSpecialMember(*MembExpr) &&
        !hasVariableWithName(*MethodDecl, *Result.Context,
                             MembExpr->getMemberDecl()->getName()) &&
        !isDependentName(*MethodDecl, *MembExpr, *Result.Context)) {

      removeExplicitThis(*Result.SourceManager, *MembExpr);
    }
  } else if ((Style == ThisStyle::Explicit) && ThisExpr->isImplicit()) {
    addExplicitThis(*ThisExpr);
  }
}
} // namespace misc
} // namespace tidy
} // namespace clang
