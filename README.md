
# emacs-dx

**Vision: A working emacs setup for developers:**
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
