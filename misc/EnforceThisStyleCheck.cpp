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
  AllowedMacroRegexp = Options.get("AllowedMacroRegexp", "");
}

void EnforceThisStyleCheck::storeOptions(ClangTidyOptions::OptionMap &Opts) {
  if (Style == ThisStyle::Implicit) {
    Options.store(Opts, "Style", "implicit");
  } else {
    Options.store(Opts, "Style", "explicit");
  }
  Options.store(Opts, "AllowedMacroRegexp", AllowedMacroRegexp);
}

static bool isTemplateInstantiation(const CXXRecordDecl &Class) {
  // Do like isTemplateInstantiation() matcher does. We don't care about
  // explicit specializations because they don't have template parameters, which
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
          // don't check compiler-generated function definitions
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

void EnforceThisStyleCheck::addExplicitThis(const MemberExpr &MembExpr) {
  const auto ThisLocation = MembExpr.getBeginLoc();
  diag(ThisLocation, "insert 'this->'")
      << FixItHint::CreateInsertion(ThisLocation, "this->");
}

static bool isNonSpecialMember(const MemberExpr &MembExpr) {
  const auto MemberDecl = MembExpr.getMemberDecl();
  assert(MemberDecl);

  const auto MethodDecl = dyn_cast<CXXMethodDecl>(MemberDecl);
  return !MethodDecl || MethodDecl->getIdentifier();
}

bool EnforceThisStyleCheck::isValidLocation(const SourceLocation ThisLocation,
                                            const SourceManager &SM) const {
  if (ThisLocation.isInvalid()) {
    return false;
  }

  if (ThisLocation.isMacroID()) {
    const auto MacroName =
        Lexer::getImmediateMacroName(ThisLocation, SM, getLangOpts());
    if (!llvm::Regex(AllowedMacroRegexp).match(MacroName)) {
      return false;
    }
  }

  return true;
}

static bool isRedundantExplicitThis(const MemberExpr &MembExpr,
                                    const CXXMethodDecl &MethodDecl,
                                    ASTContext &Context) {
  return (isNonSpecialMember(MembExpr) &&
          !hasVariableWithName(MethodDecl, Context,
                               MembExpr.getMemberDecl()->getName()) &&
          !isDependentName(MethodDecl, MembExpr, Context));
}

void EnforceThisStyleCheck::check(const MatchFinder::MatchResult &Result) {
  const auto ThisExpr = Result.Nodes.getNodeAs<CXXThisExpr>("thisExpr");
  assert(ThisExpr);

  if (!isValidLocation(ThisExpr->getLocation(), *Result.SourceManager)) {
    return;
  }

  const auto MembExpr = Result.Nodes.getNodeAs<MemberExpr>("memberExpr");
  assert(MembExpr);

  if ((Style == ThisStyle::Implicit) && !ThisExpr->isImplicit()) {
    const auto MethodDecl = Result.Nodes.getNodeAs<CXXMethodDecl>("methodDecl");
    assert(MethodDecl);

    if (isRedundantExplicitThis(*MembExpr, *MethodDecl, *Result.Context)) {
      removeExplicitThis(*Result.SourceManager, *MembExpr);
    }
  } else if ((Style == ThisStyle::Explicit) && ThisExpr->isImplicit()) {
    addExplicitThis(*MembExpr);
  }
}
} // namespace misc
} // namespace tidy
} // namespace clang
