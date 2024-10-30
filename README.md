
# emacs-dx

## Development Log

**2024/10/30 9:55**
* Cleaned up init, ui, and python el files using donations from wunderfitz (thank you)
* Testing and validating

**2024/10/30**
Made updates to [dx-python-dev.el](https://github.com/gregfelice/emacs-dx/blob/main/emacs.d/emacs-dx/dx-python-dev.el)
* Uses a treesitter config
* Configures Eglot
* Handles some basics, but still a long way to go
* Kind people are donating their .el files, looking to integrate, test, and post what I experience


## Vision: An emacs setup for developers that just works

- A front end and back end developer profile: python AI and data science, react/next.js, based on "current good practice" env and build tooling.
- Emacs configuration choices for you so you can get running
- Documentation what options were considered, what was ruled out, and why

**Credits**
- Much of the boilerplate to get started is from [emacs-bedrock](https://github.com/ashton314/emacs-bedrock)

**Status**
- Prototype is working on Mac + Emacs 29.4, but in heavy iteration as I learn. Consider it unstable and pre-release. I'm sharing it for feedback.

**Setup**
- brew install emacs
- git clone https://github.com/gregfelice/emacs-dx.git
- $ emacs --init-directory $HOME/emacs-dx/emacs-d      # temporary location so you can try it out without disturbing your .emacs.d

**TODO**
- Consolidate el files
- Prog Mode
- Flymake instead of Flycheck
- Corfu instead of Company - But what about Eglot?
- Elist comments
- LSP addon configs, mypy and flake8
- Test on linux
- Test on windows
- React developer profile
