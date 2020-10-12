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

AST_MATCHER(ClassTemplateSpecializationDecl, isExplicitSpecialization) {
  return Node.isExplicitSpecialization();
}

EnforceThisStyleCheck::EnforceThisStyleCheck(StringRef Name,
                                             ClangTidyContext *Context)
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

template <typename NodeType>
static bool isInInstantiation(const NodeType &Node,
                              const MatchFinder::MatchResult &Result) {
  return !match(isInTemplateInstantiation(), Node, *Result.Context).empty();
}

template <typename NodeType>
static bool isInTemplate(const NodeType &Node,
                         const MatchFinder::MatchResult &Result) {
  auto IsInsideTemplate = anyOf(hasAncestor(classTemplateDecl()),
                                hasAncestor(classTemplateSpecializationDecl(
                                    unless(isExplicitSpecialization()))));
  return !match(decl(IsInsideTemplate), Node, *Result.Context).empty();
}

void EnforceThisStyleCheck::registerMatchers(MatchFinder *Finder) {
  if (!getLangOpts().CPlusPlus)
    return;

  // Finder->addMatcher(
  //     cxxMethodDecl(
  //         isDefinition(),
  //         forEachDescendant(
  //             memberExpr(has(ignoringImpCasts(cxxThisExpr().bind("thisExpr"))))
  //                 .bind("memberExpr")))
  //         .bind("method"),
  //     this);
  Finder->addMatcher(
      cxxMethodDecl(
          isDefinition(),
          forEachDescendant(expr(eachOf(
              memberExpr(has(ignoringImpCasts(cxxThisExpr().bind("thisExpr"))))
                  .bind("memberExpr"),
              cxxDependentScopeMemberExpr(
                  has(ignoringImpCasts(cxxThisExpr().bind("thisExpr"))))
                  .bind("depMemberExpr"),
              unresolvedMemberExpr(
                  has(ignoringImpCasts(cxxThisExpr().bind("thisExpr"))))
                  .bind("unresMemberExpr")))))
          .bind("method"),
      this);
}

bool hasVariableWithName(const CXXMethodDecl &Function, ASTContext &Context,
                         StringRef Name) {
  const auto Matches =
      match(decl(hasDescendant(varDecl(hasName(Name)).bind("var"))), Function,
            Context);

  return !Matches.empty();
}

bool hasDirectMember(const CXXRecordDecl &Record, ASTContext &Context,
                     StringRef Name) {
  // if(Name == "Parse"){
  //   Record.dumpColor();
  // }
  const auto Matches =
      match(cxxRecordDecl(has(namedDecl(hasName(Name)))), Record, Context);
  // match(cxxRecordDecl(anyOf(has(fieldDecl(hasName(Name))),
  //                           has(cxxMethodDecl(hasName(Name))))),
  //       Record, Context);

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

void EnforceThisStyleCheck::removeExplicitThis(
    const SourceManager &SM, const CXXDependentScopeMemberExpr &MembExpr) {
  const auto ThisStart = MembExpr.getBeginLoc();
  const auto ThisEnd = MembExpr.getMemberLoc();
  const auto ThisRange = Lexer::makeFileCharRange(
      CharSourceRange::getCharRange(ThisStart, ThisEnd), SM, getLangOpts());

  diag(MembExpr.getBeginLoc(), "remove 'this->'", DiagnosticIDs::Note)
      << FixItHint::CreateRemoval(ThisRange);
}

void EnforceThisStyleCheck::removeExplicitThis(
    const SourceManager &SM, const UnresolvedMemberExpr &MembExpr) {
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

static bool isSimpleMember(const CXXDependentScopeMemberExpr &MembExpr) {
  return MembExpr.getMember().getAsIdentifierInfo();
}

static bool isSimpleMember(const UnresolvedMemberExpr &MembExpr) {
  return MembExpr.getMemberName().getAsIdentifierInfo();
}

static bool isSingleMacroArgument(SourceLocation Loc, const SourceManager &SM) {
  assert(Loc.isMacroID() && "Only reasonable to call this on macros");

  FileID FID = SM.getFileID(Loc);
  const SrcMgr::SLocEntry *E = &SM.getSLocEntry(FID);
  const SrcMgr::ExpansionInfo &Expansion = E->getExpansion();
  Loc = Expansion.getExpansionLocStart();
  if (Expansion.isMacroArgExpansion()) {
    // For macro arguments we need to check that the argument did not come
    // from an inner macro, e.g: "MAC1( MAC2(foo) )"

    // Loc points to the argument id of the macro definition, move to the
    // macro expansion.
    Loc = SM.getImmediateExpansionRange(Loc).getBegin();
    SourceLocation SpellLoc = Expansion.getSpellingLoc();
    if (SpellLoc.isFileID()) {
      // No inner macro.
      return true;
    }
    // If spelling location resides in the same FileID as macro expansion
    // location, it means there is no inner macro.
    FileID MacroFID = SM.getFileID(Loc);
    if (SM.isInFileID(SpellLoc, MacroFID)) {
      return true;
    }
  } else {
    llvm::outs() << "isMacroArgExpansion() = false \n";
  }

  return false;
}

void EnforceThisStyleCheck::check(const MatchFinder::MatchResult &Result) {
  const auto MatchedThis = Result.Nodes.getNodeAs<CXXThisExpr>("thisExpr");
  assert(MatchedThis);

  const auto ThisLocation = MatchedThis->getLocation();

  if (ThisLocation.isInvalid() ||
      (ThisLocation.isMacroID() &&
       (Lexer::getImmediateMacroName(ThisLocation, *Result.SourceManager,
                                     getLangOpts()) != "assert"))) {
    return;
  }

  const auto MatchedMember = Result.Nodes.getNodeAs<MemberExpr>("memberExpr");

  if ((Style == ThisStyle::Implicit) && !MatchedThis->isImplicit()) {
    const auto MatchedMethod = Result.Nodes.getNodeAs<CXXMethodDecl>("method");
    assert(MatchedMethod);

    if (MatchedMember && !isInTemplate(*MatchedMethod, Result)) {
      // methods in non-template classes or instantiated methods
      if (isSimpleMember(*MatchedMember) && !MatchedMember->hasQualifier() &&
          !hasVariableWithName(*MatchedMethod, *Result.Context,
                               MatchedMember->getMemberDecl()->getName())) {
        diag(MatchedThis->getLocation(),
             "explicit `this->` detected | NON TMP | ");
        removeExplicitThis(*Result.SourceManager, *MatchedMember);
      }
    } else {
      const auto MatchedDepMember =
          Result.Nodes.getNodeAs<CXXDependentScopeMemberExpr>("depMemberExpr");
      const auto MatchedUnresMember =
          Result.Nodes.getNodeAs<UnresolvedMemberExpr>("unresMemberExpr");
      if (MatchedDepMember) {
        // method in template definition
        const auto Class = MatchedMethod->getParent();
        assert(Class);

        const auto hasQualifier = !!MatchedDepMember->getQualifier();
        const auto isSimpleName = isSimpleMember(*MatchedDepMember);

        // if (isSimpleName) {
        //   llvm::outs()
        //       << "TMP candidate | "
        //       << MatchedDepMember->getMember().getAsIdentifierInfo()->getName()
        //       << '\n';
        // }

        const auto hasDirMember =
            isSimpleName &&
            hasDirectMember(
                *Class, *Result.Context,
                MatchedDepMember->getMember().getAsIdentifierInfo()->getName());
        const auto hasSameLocalName =
            isSimpleName &&
            hasVariableWithName(
                *MatchedMethod, *Result.Context,
                MatchedDepMember->getMember().getAsIdentifierInfo()->getName());

        if (!hasQualifier && isSimpleName && hasDirMember &&
            !hasSameLocalName) {
          diag(MatchedThis->getLocation(),
               "explicit `this->` detected | TMP |");
          removeExplicitThis(*Result.SourceManager, *MatchedDepMember);
        } else if (isSimpleName) {
          // llvm::outs()
          //     << "TMP candidate FAILED | "
          //     << MatchedDepMember->getMember().getAsIdentifierInfo()->getName()
          //     << " | !hasQualifier " << !hasQualifier << " | isSimpleName"
          //     << isSimpleName << " | hasDirMember" << hasDirMember
          //     << " | !hasSameLocalName" << !hasSameLocalName << '\n';
        }
      } else if (MatchedUnresMember) {
        // method in template definition
        const auto Class = MatchedMethod->getParent();
        assert(Class);

        const auto hasQualifier = !!MatchedUnresMember->getQualifier();
        const auto isSimpleName = isSimpleMember(*MatchedUnresMember);

        // if (isSimpleName) {
        //   llvm::outs() << "TMP candidate | "
        //                << MatchedUnresMember->getMemberName()
        //                       .getAsIdentifierInfo()
        //                       ->getName()
        //                << '\n';
        // }

        const auto hasDirMember =
            isSimpleName && hasDirectMember(*Class, *Result.Context,
                                            MatchedUnresMember->getMemberName()
                                                .getAsIdentifierInfo()
                                                ->getName());
        const auto hasSameLocalName =
            isSimpleName &&
            hasVariableWithName(*MatchedMethod, *Result.Context,
                                MatchedUnresMember->getMemberName()
                                    .getAsIdentifierInfo()
                                    ->getName());

        if (!hasQualifier && isSimpleName && hasDirMember &&
            !hasSameLocalName) {
          diag(MatchedThis->getLocation(),
               "explicit `this->` detected | TMP |");
          removeExplicitThis(*Result.SourceManager, *MatchedUnresMember);
        }
      }
    }
  } else if ((Style == ThisStyle::Explicit) && MatchedThis->isImplicit()) {
    // with implicit this only MemberExpr is possible
    assert(MatchedMember);
    if (!MatchedMember->hasQualifier()) {
      diag(MatchedThis->getLocation(), "implicit `this->` detected");
      addExplicitThis(*MatchedThis);
    }
  }
}
} // namespace misc
} // namespace tidy
} // namespace clang
