# Smoke tests for CI/CD
{ pkgs
, kookyEmacs
, ...
}:

pkgs.runCommand "kooky-smoke-tests"
{
  buildInputs = [ kookyEmacs ];
} ''
  # Test that Emacs loads
  ${kookyEmacs}/bin/emacs --batch --eval "(message \"Emacs loads successfully\")" 2>&1 | tee test-output.txt
  grep -q "Emacs loads successfully" test-output.txt

  # Test that use-package is available
  ${kookyEmacs}/bin/emacs --batch --eval "(require 'use-package) (message \"use-package available\")" 2>&1 | tee test-output.txt
  grep -q "use-package available" test-output.txt

  touch $out
  echo "Smoke tests passed!"
''
