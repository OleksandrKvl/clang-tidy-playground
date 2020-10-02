add_clang_library(clangTidyMiscModule
  ExplicitThisCheck.cpp
  MiscTidyModule.cpp
  NonDataStructsCheck.cpp

  LINK_LIBS
  clangAnalysis
  clangAST
  clangASTMatchers
  clangBasic
  clangLex
  clangSerialization
  clangTidy
  clangTidyUtils
  clangTooling
  )
