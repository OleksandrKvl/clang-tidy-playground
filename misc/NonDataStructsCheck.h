//===--- NonDataStructsCheck.h - clang-tidy ---------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_MISC_NONDATASTRUCTSCHECK_H
#define LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_MISC_NONDATASTRUCTSCHECK_H

#include "../ClangTidyCheck.h"

namespace clang {
namespace tidy {
namespace misc {

/// This check detects struct-s that has either non-static member functions,
/// non-public data members, non-public or non-struct bases, static data members
/// are allowed.
class NonDataStructsCheck : public ClangTidyCheck {
public:
  enum class AllowedCtorKind { None, Default, Primary };

  NonDataStructsCheck(StringRef Name, ClangTidyContext *Context);
  void registerMatchers(ast_matchers::MatchFinder *Finder) override;
  void check(const ast_matchers::MatchFinder::MatchResult &Result) override;
  void storeOptions(ClangTidyOptions::OptionMap &Opts) override;

private:
  bool AllowNonEmptyCtorBody;
  bool AllowDefaultMemberInit;
  AllowedCtorKind AllowedCtors;
  bool SkipStateless;

  static AllowedCtorKind StringToCtorKind(StringRef Str);
  static std::string CtorKindToString(AllowedCtorKind Kind);
};

} // namespace misc
} // namespace tidy
} // namespace clang

#endif // LLVM_CLANG_TOOLS_EXTRA_CLANG_TIDY_MISC_NONDATASTRUCTSCHECK_H
