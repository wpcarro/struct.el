{ depot, ... }:

depot.tools.emacs-pkgs.buildEmacsPackage {
  pname = "struct";
  version = "1.0.0";
  src = ./struct.el;
  doInstallCheck = true;
  installCheckPhase = ''
    emacs -batch \
      -l ert -l ${./tests.el} -f ert-run-tests-batch-and-exit
  '';
  meta.ci.extraSteps.github = depot.tools.releases.filteredGitPush {
    filter = ":/users/wpcarro/emacs/pkgs/struct";
    remote = "git@github.com:wpcarro/struct.el.git";
    ref = "refs/heads/canon";
  };
}
