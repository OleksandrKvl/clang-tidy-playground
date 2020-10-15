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

// template <typename NodeType>
// static bool isInInstantiation(const NodeType &Node,
//                               const MatchFinder::MatchResult &Result) {
//   return !match(isInTemplateInstantiation(), Node, *Result.Context).empty();
// }

template <typename NodeType>
static bool isInTemplate(const NodeType &Node,
                         const MatchFinder::MatchResult &Result) {
  // auto IsInsideTemplate = anyOf(hasAncestor(classTemplateDecl()),
  //                               hasAncestor(classTemplateSpecializationDecl(
  //                                 // classTemplateSpecializationDecl is
  //                                 always a full
  //                                 // specialization, explicit specialization
  //                                 means
  //                                 // it's a hand-written one, it doesn't
  //                                 contain
  //                                 // template parameters
  //                                   unless(isExplicitSpecialization()))));

  // classTemplateSpecializationDecl is always a full specialization, explicit
  // specialization means it's a hand-written one, it doesn't have template
  // parameters
  // auto IsInsideTemplate = hasAncestor(
  //     classTemplateSpecializationDecl(unless(isExplicitSpecialization())));

  auto IsInsideTemplate = hasAncestor(classTemplateSpecializationDecl(
      unless(isExplicitTemplateSpecialization())));

  return !match(decl(IsInsideTemplate), Node, *Result.Context).empty();
}

static bool isInTemplate(const CXXMethodDecl &Method,
                         const MatchFinder::MatchResult &Result) {
  // classTemplateSpecializationDecl is always a full specialization, explicit
  // specialization means it's a hand-written one, it doesn't have template
  // parameters

  // auto IsInsideTemplate = hasAncestor(classTemplateSpecializationDecl(
  //     unless(isExplicitTemplateSpecialization())));
  // const auto MatchResult =
  //     !match(decl(IsInsideTemplate), Method, *Result.Context).empty();

  // do like isTemplateInstantiation() matcher does
  const auto Class = Method.getParent();
  const auto SpecKind = Class->getTemplateSpecializationKind();
  const auto IsInstantiated = (SpecKind == TSK_ImplicitInstantiation ||
                       SpecKind == TSK_ExplicitInstantiationDefinition ||
                       SpecKind == TSK_ExplicitInstantiationDeclaration);

  return IsInstantiated;
}

void EnforceThisStyleCheck::registerMatchers(MatchFinder *Finder) {
  if (!getLangOpts().CPlusPlus) {
    return;
  }
  // handle template version
  // Finder->addMatcher(
  //     cxxMethodDecl(
  //         isDefinition(),
  //         unless(isDefaulted()),
  //         forEachDescendant(expr(eachOf(
  //             memberExpr(has(ignoringImpCasts(cxxThisExpr().bind("thisExpr"))))
  //                 .bind("memberExpr"),
  //             cxxDependentScopeMemberExpr(
  //                 has(ignoringImpCasts(cxxThisExpr().bind("thisExpr"))))
  //                 .bind("depMemberExpr"),
  //             unresolvedMemberExpr(
  //                 has(ignoringImpCasts(cxxThisExpr().bind("thisExpr"))))
  //                 .bind("unresMemberExpr")))))
  //         .bind("method"),
  //     this);

  // handle only instantiations version
  Finder->addMatcher(
      cxxMethodDecl(
          // don't check compiler-generated function bodies
          isDefinition(), unless(anyOf(isImplicit(), isDefaulted())),
          forEachDescendant(
              memberExpr(has(ignoringImpCasts(cxxThisExpr().bind("thisExpr"))))
                  .bind("memberExpr")))
          .bind("method"),
      this);
}

bool hasVariableWithName(const CXXMethodDecl &Function, ASTContext &Context,
                         StringRef Name) {
  const auto Matches =
      match(decl(hasDescendant(varDecl(hasName(Name)))), Function, Context);

  return !Matches.empty();
}

bool hasDirectMember(const CXXRecordDecl &Record, ASTContext &Context,
                     StringRef Name) {
  const auto Matches =
      match(cxxRecordDecl(has(namedDecl(hasName(Name)))), Record, Context);

  return !Matches.empty();
}

void EnforceThisStyleCheck::removeExplicitThis(SourceLocation ThisStart,
                                               SourceLocation ThisEnd,
                                               const SourceManager &SM) {
  const auto ThisRange = Lexer::makeFileCharRange(
      CharSourceRange::getCharRange(ThisStart, ThisEnd), SM, getLangOpts());

  diag(ThisStart, "remove 'this->'", DiagnosticIDs::Warning)
      << FixItHint::CreateRemoval(ThisRange);
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

void EnforceThisStyleCheck::removeExplicitThis(
    const SourceManager &SM, const CXXDependentScopeMemberExpr &MembExpr) {
  const auto ThisStart = MembExpr.getBeginLoc();
  auto ThisEnd = MembExpr.getMemberLoc();
  if (MembExpr.getQualifier()) {
    ThisEnd = MembExpr.getQualifierLoc().getBeginLoc();
  }

  removeExplicitThis(ThisStart, ThisEnd, SM);
}

void EnforceThisStyleCheck::removeExplicitThis(
    const SourceManager &SM, const UnresolvedMemberExpr &MembExpr) {
  const auto ThisStart = MembExpr.getBeginLoc();
  auto ThisEnd = MembExpr.getMemberLoc();
  if (MembExpr.getQualifier()) {
    ThisEnd = MembExpr.getQualifierLoc().getBeginLoc();
  }

  removeExplicitThis(ThisStart, ThisEnd, SM);
}

void EnforceThisStyleCheck::addExplicitThis(const MemberExpr &ThisExpr) {
  diag(ThisExpr.getBeginLoc(), "insert 'this->'", DiagnosticIDs::Note)
      << FixItHint::CreateInsertion(ThisExpr.getBeginLoc(), "this->");
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
  assert(MatchedMember);

  if ((Style == ThisStyle::Implicit) && !MatchedThis->isImplicit()) {
    const auto MatchedMethod = Result.Nodes.getNodeAs<CXXMethodDecl>("method");
    assert(MatchedMethod);

    // if in template - check for direct members
    const auto Class = MatchedMethod->getParent();
    assert(Class);

    if (isSimpleMember(*MatchedMember) &&
        !hasVariableWithName(*MatchedMethod, *Result.Context,
                             MatchedMember->getMemberDecl()->getName()) &&
        (!isInTemplate(*MatchedMethod, Result) ||
         hasDirectMember(*Class, *Result.Context,
                         MatchedMember->getMemberDecl()->getName()))) {
      // diag(MatchedThis->getLocation(), "explicit `this->` detected");

      removeExplicitThis(*Result.SourceManager, *MatchedMember);
    }
  } else if ((Style == ThisStyle::Explicit) && MatchedThis->isImplicit()) {
    // with implicit this only MemberExpr is possible
    assert(MatchedMember);

    // diag(MatchedMember->getBeginLoc(), "implicit `this->` detected");
    addExplicitThis(*MatchedMember);
  }
}
// void EnforceThisStyleCheck::check(const MatchFinder::MatchResult &Result) {
//   const auto MatchedThis = Result.Nodes.getNodeAs<CXXThisExpr>("thisExpr");
//   assert(MatchedThis);

//   const auto ThisLocation = MatchedThis->getLocation();

//   if (ThisLocation.isInvalid() ||
//       (ThisLocation.isMacroID() &&
//        (Lexer::getImmediateMacroName(ThisLocation, *Result.SourceManager,
//                                      getLangOpts()) != "assert"))) {
//     return;
//   }

//   const auto MatchedMember =
//   Result.Nodes.getNodeAs<MemberExpr>("memberExpr");

//   if ((Style == ThisStyle::Implicit) && !MatchedThis->isImplicit()) {
//     const auto MatchedMethod =
//     Result.Nodes.getNodeAs<CXXMethodDecl>("method"); assert(MatchedMethod);

//     if (MatchedMember && !MatchedMethod->isTemplateInstantiation() &&
//         !isInTemplate(*MatchedMethod, Result)) {
//       if (isSimpleMember(*MatchedMember)) {
//         // llvm::outs() << "NON TMP candidate | "
//         //              << MatchedMember->getMemberDecl()->getName()
//         //              << " | !hasQualifier(): " <<
//         //              !MatchedMember->hasQualifier()
//         //              << " | !hasVariableWithName(): "
//         //              << !hasVariableWithName(
//         //                     *MatchedMethod, *Result.Context,
//         //                     MatchedMember->getMemberDecl()->getName())
//         //              << '\n';
//       }
//       // methods in non-template classes or instantiated methods
//       if (isSimpleMember(*MatchedMember) /*&&
//       !MatchedMember->hasQualifier()*/
//           && !hasVariableWithName(*MatchedMethod, *Result.Context,
//                                   MatchedMember->getMemberDecl()->getName()))
//                                   {
//         diag(MatchedThis->getLocation(),
//              "explicit `this->` detected | NON TMP");

//         // MatchedMethod->dumpColor();
//         removeExplicitThis(*Result.SourceManager, *MatchedMember);
//       }
//     } else {
//       const auto MatchedDepMember =
//           Result.Nodes.getNodeAs<CXXDependentScopeMemberExpr>("depMemberExpr");
//       const auto MatchedUnresMember =
//           Result.Nodes.getNodeAs<UnresolvedMemberExpr>("unresMemberExpr");
//       if (MatchedDepMember) {
//         // method in template definition
//         const auto Class = MatchedMethod->getParent();
//         assert(Class);

//         const auto isSimpleName = isSimpleMember(*MatchedDepMember);

//         // if (isSimpleName) {
//         //   llvm::outs()
//         //       << "TMP candidate | "
//         //       <<
//         // MatchedDepMember->getMember().getAsIdentifierInfo()->getName()
//         //       << '\n';
//         // }

//         const auto hasDirMember =
//             isSimpleName &&
// hasDirectMember(
//     *Class, *Result.Context,
//     MatchedDepMember->getMember().getAsIdentifierInfo()->getName());
//         const auto hasSameLocalName =
//             isSimpleName &&
//             hasVariableWithName(
//                 *MatchedMethod, *Result.Context,
//                 MatchedDepMember->getMember().getAsIdentifierInfo()->getName());

//         if (isSimpleName && hasDirMember && !hasSameLocalName) {
//           diag(MatchedThis->getLocation(),
//                "explicit `this->` detected | TMP |");
//           removeExplicitThis(*Result.SourceManager, *MatchedDepMember);
//         } else if (isSimpleName) {
//           // llvm::outs()
//           //     << "TMP candidate FAILED | "
//           //     <<
//           // MatchedDepMember->getMember().getAsIdentifierInfo()->getName()
//           //     << " | !hasQualifier " << !hasQualifier << " | isSimpleName"
//           //     << isSimpleName << " | hasDirMember" << hasDirMember
//           //     << " | !hasSameLocalName" << !hasSameLocalName << '\n';
//         }
//       } else if (MatchedUnresMember) {
//         // method in template definition
//         const auto Class = MatchedMethod->getParent();
//         assert(Class);

//         const auto isSimpleName = isSimpleMember(*MatchedUnresMember);

//         if (isSimpleName) {
//           llvm::outs() << "TMP candidate | "
//                        << MatchedUnresMember->getMemberName()
//                               .getAsIdentifierInfo()
//                               ->getName()
//                        << '\n';
//         }

//         const auto hasDirMember =
//             isSimpleName && hasDirectMember(*Class, *Result.Context,
//                                             MatchedUnresMember->getMemberName()
//                                                 .getAsIdentifierInfo()
//                                                 ->getName());
//         const auto hasSameLocalName =
//             isSimpleName &&
//             hasVariableWithName(*MatchedMethod, *Result.Context,
//                                 MatchedUnresMember->getMemberName()
//                                     .getAsIdentifierInfo()
//                                     ->getName());

//         if (isSimpleName && hasDirMember && !hasSameLocalName) {
//           diag(MatchedThis->getLocation(),
//                "explicit `this->` detected | TMP |");
//           removeExplicitThis(*Result.SourceManager, *MatchedUnresMember);
//         }
//       }
//     }
//   } else if ((Style == ThisStyle::Explicit) && MatchedThis->isImplicit()) {
//     // with implicit this only MemberExpr is possible
//     assert(MatchedMember);

//     diag(MatchedMember->getBeginLoc(), "implicit `this->` detected");
//     addExplicitThis(*MatchedMember);
//   }
// }
} // namespace misc
} // namespace tidy
} // namespace clang
