# Homebrew formula for First compiler (LLVM-based).
# Install from tap: brew tap akkeshavan/first && brew install first-compiler
# Or install this file: brew install --build-from-source ./homebrew/first-compiler.rb
# Uninstall: brew uninstall first-compiler

class FirstCompiler < Formula
  desc "First programming language compiler (LLVM-based)"
  homepage "https://github.com/akkeshavan/first"
  head "https://github.com/akkeshavan/first.git", branch: "main"
  version "0.1.0"

  depends_on "cmake" => :build
  depends_on "llvm"
  depends_on "antlr4-cpp-runtime"
  depends_on "antlr" => :build  # antlr4 command for grammar generation

  def install
    # ANTLR4 tool must be on PATH for CMake to generate lexer
    antlr_bin = Formula["antlr"].opt_bin
    ENV.prepend_path "PATH", antlr_bin

    system "cmake", "-S", ".", "-B", "build",
                    "-DCMAKE_BUILD_TYPE=Release",
                    "-DCMAKE_INSTALL_PREFIX=#{prefix}",
                    "-DBUILD_TESTS=OFF",
                    "-DBUILD_EXAMPLES=OFF",
                    "-DFIRST_USE_GC=OFF"
    system "cmake", "--build", "build"
    system "cmake", "--install", "build"
  end

  test do
    (testpath/"hello.first").write <<~EOS
      interaction main() {
        println("Hello from First");
      }
    EOS
    system bin/"firstc", "hello.first", "-o", "hello"
    assert_predicate testpath/"hello", :exist?
  end
end
