//===--- EnforceThisStyleCheck.h - clang-tidy -------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_MISC_ENFORCETHISSTYLECHECK_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_MISC_ENFORCETHISSTYLECHECK_H

#include "../ClangTidyCheck.h"

namespace clang {
namespace tidy {
namespace misc {

/// FIXME: Write a short description.
///
/// For the user-facing documentation see:
/// http://clang.llvm.org/extra/clang-tidy/checks/misc-enforce-this-style.html
class EnforceThisStyleCheck : public ClangTidyCheck {
public:
  EnforceThisStyleCheck(StringRef Name, ClangTidyContext *Context);
  void registerMatchers(ast_matchers::MatchFinder *Finder) override;
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override;
  void storeOptions(ClangTidyOptions::OptionMap &Opts) override;

  bool isValidLocation(SourceLocation ThisLocation,
                       const SourceManager &SM) const;
  void addExplicitThis(const MemberExpr &MembExpr);
  void removeExplicitThis(const SourceManager &SM,
                          const MemberExpr &MatchedMember);

private:
  enum class ThisStyle { Implicit, Explicit };
  ThisStyle Style{ThisStyle::Implicit};
  std::string AllowedMacroRegexp;
};

} // namespace misc
} // namespace tidy
} // namespace clang

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_MISC_ENFORCETHISSTYLECHECK_H
