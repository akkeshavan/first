# Homebrew formula for First compiler (LLVM-based).
#
# Full install: firstc (PREFIX/bin), stdlib (PREFIX/lib/first/), runtime (PREFIX/lib/libfirst_runtime.a).
# After install, firstc finds Prelude and the runtime via the "lib next to binary" paths.
#
# Install from tap:
#   brew tap akkeshavan/first
#   brew install first-compiler
#
# Install from local formula (no tap):
#   brew install --build-from-source ./homebrew/first-compiler.rb
#
# Install latest from main:
#   brew install --HEAD akkeshavan/first/first-compiler
#
# Uninstall: brew uninstall first-compiler

class FirstCompiler < Formula
  desc "First programming language compiler (LLVM-based)"
  homepage "https://github.com/akkeshavan/first"
  version "0.1.0"

  # Head-only: install with `brew install --HEAD akkeshavan/first/first-compiler`
  # To add a stable release: add url/sha256 for a tag (e.g. v0.1.0) and keep head for dev.
  head "https://github.com/akkeshavan/first.git", branch: "main"

  depends_on "cmake" => :build
  depends_on "llvm"
  depends_on "antlr4-cpp-runtime"
  depends_on "antlr" => :build
  depends_on "bdw-gc"  # compiler links with -lgc when producing executables

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
    # Installs: bin/firstc, lib/first/*.first (stdlib), lib/libfirst_runtime.a
    system "cmake", "--install", "build", "--component", "first"

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
