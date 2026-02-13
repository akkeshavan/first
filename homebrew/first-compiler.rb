# Homebrew formula for First compiler (LLVM-based).
#
# Full install: firstc (PREFIX/bin), stdlib (PREFIX/lib/first/), runtime (PREFIX/lib/libfirst_runtime.a).
# After install, firstc finds Prelude and the runtime via the "lib next to binary" paths.
#
# Install:
#   brew tap akkeshavan/first
#   brew install --HEAD first-compiler
#
# Uninstall: brew uninstall first-compiler

class FirstCompiler < Formula
  desc "First programming language compiler (LLVM-based)"
  homepage "https://github.com/akkeshavan/first"
  version "0.1.0"

  head "https://github.com/akkeshavan/first.git", branch: "main"

  depends_on "cmake" => :build
  depends_on "llvm"
  depends_on "antlr4-cpp-runtime"
  depends_on "antlr" => :build
  depends_on "bdw-gc"

  def install
    # ANTLR4 tool must be on PATH for CMake to generate lexer
    antlr_bin = Formula["antlr"].opt_bin
    ENV.prepend_path "PATH", antlr_bin

    system "cmake", "-S", ".", "-B", "build",
                    "-DCMAKE_BUILD_TYPE=Release",
                    "-DCMAKE_INSTALL_PREFIX=#{prefix}",
                    "-DBUILD_TESTS=OFF",
                    "-DBUILD_EXAMPLES=OFF",
                    "-DFIRST_USE_GC=ON"
    system "cmake", "--build", "build"
    # Installs: bin/firstc, lib/first/*.first (stdlib), lib/libfirst_runtime.a
    system "cmake", "--install", "build", "--component", "first"

    # On Linux, firstc can segfault on load (LLVM/ANTLR static init). Use a wrapper
    # so --version and --help work without invoking the heavy binary.
    if OS.linux?
      libexec.install bin/"firstc" => "firstc.real"
      (bin/"firstc").write <<~EOS
        #!/bin/bash
        case "${1:-}" in
          --version|-v)
            echo "firstc version 0.1.0"
            echo "First Programming Language Compiler"
            exit 0
            ;;
          --help|-h)
            echo "First Programming Language Compiler"
            echo "Usage: firstc [options] <source_file>"
            echo ""
            echo "Options:"
            echo "  -o <file>        Output file: .o = object only; .ll = LLVM IR; else executable"
            echo "  --version, -v    Print version information"
            echo "  --help, -h       Print this help message"
            echo "  --verbose        Enable verbose output"
            echo ""
            exit 0
            ;;
          "")
            echo "Error: No source file specified"
            echo "Usage: firstc [options] <source_file>"
            exit 1
            ;;
        esac
        exec "#{libexec}/firstc.real" "$@"
      EOS
      chmod 0755, bin/"firstc"
    end

    # Install fir (First project manager) if present
    fir = "tools/fir"
    bin.install fir => "fir" if File.exist?(fir)
  end

  test do
    (testpath/"hello.first").write <<~EOS
      import "Prelude"
      interaction main() -> Unit {
        println("Hello from First");
      }
    EOS
    system bin/"firstc", "hello.first", "-o", "hello"
    assert_predicate testpath/"hello", :exist?
    assert_equal "Hello from First\n", shell_output("./hello")
  end
end
