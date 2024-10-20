
# emacs-dx / python developer profile

## the python developer persona

### description
- studying to be professional developer or already professional developer
- comfortable with venv or anaconda
- not an expert in sysadmin, but comfortable with command shells
- uses source control / git

### goals
- get productive quickly
- have an enjoyable coding experience

### critical user journeys & tasks
- set up new project
  - i do this at zsh prompt with venv and pip
- develop web application
- develop ML model
- manage & analyze data
- deploy to different environments
- monitor systems
- communicate with people
  - write & preview markdown
  - post markdown to gh
  - use gmail, whatsapp, google chat, zoom, google calendar
- customize my development setup
  - change iterm themes & fonts
  - change emacs themes

## conditions of satisfaction
- coding support works oob
- works on windows, mac, debian variants
- installs and configures language server


## design

- bedrock as foundation el
- build tailored enablement for python developer on top


[GNU Emacs Documentation](https://www.gnu.org/software/emacs/documentation.html)

| Capability             | Solution                                      | Supported for Python? |  
|------------------------|-----------------------------------------------|-----------------------|
| Syntax Checking        | Flymake                                       |                  | 
| At-Point Documentation | Eglot                                         | 
| Diagnostic Annotations | Eglot, via Flymake                            |
| Identifier Definitions | Eglot, via Xref                               |
| Buffer Navigation      | Eglot, via IMenu                              |
| At-Point Completion    | Eglot, via Language Server                    |
| Automatic Formatting   | Eglot                                         |
| Completion             | Eglot, via Language Server and Company Mode   |
| Code Insertion         | Eglot, yasnippet and Language Server          |
| At-Point Documentation | Eglot, via ElDoc                              | 


### Eglot
- [Eglot Commands](https://www.gnu.org/software/emacs/manual/html_mono/eglot.html#Eglot-Commands) for more details

### Company Mode
- https://company-mode.github.io/
- available on ELPA


### Language Server (Python)
- pyright
  - cross-platform (mac, linux, windows)
  - https://github.com/emacs-lsp/lsp-pyright
  - pyright settings: https://github.com/microsoft/pyright/blob/main/docs/settings.md

language server installation
- will automatically start pyright when python files are opened in emacs.


### Out of Scope

Treesitter
- https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
- It looks like treesitter is still somewhat bleeding edge.



## user research (what the user exeperiences)

### 2024.19.10 - scenario: simulating python developer environment setup, using vanilla emacs & bedrock

given:
- i am an experienced python developer, and understand command lines and use zsh
- i have already configured a python development project using pip and venv
- i am installing & configuring emacs with bedrock for a better experience
- i have only basic exposure to emacs, but i've heard it's worth checking out

what i see:
- bedrock has good startup times and clean el
- i don't understand most of the emacs related terms in the comments: eglot? flymake?
- there's a lot of externalities needed to get this working
- googling these terms doesn't really point to any clear guidance

what i think & feel:
- in this scenario, i am unsuccessful configuring a working environment
- i've just spent an hour trying to get this to work
- i'm frustrated

analysis:
- bedrock can be an accelerant leading to a good experience for python
- work needs to be done to get to a good python development ux out of the box
- external dependency guidance (and hopefully more automated setup) is critical

step by step log:

- start
- install emacs from brew
- git clone bedrock
- uncommented dev.el in bedrock init.el
- uncommenting eglot in dev.el
- got treesitter error about language grammar unavailable
- running M-x treesit-install-language-grammar to install language
- get question: There is no recipe for python, do you want to build it interactively? (y or n) - choosing yes, taking default repo and branch
- asked: Enter the subdirectory in which the parser.c file resides (default: "src"):
- ctl-v and ctl-z dont work in emacs... -- seems to be mapped to mac standard keys. i like that, but it's jarring and i don't know where to configure
- says treesitter language is installed
- warning: Can’t guess python-indent-offset, using defaults: 4
- open im.py
- no indications of any coding support activated
- enabled treesit-inspect-mode via M-x
- seems to show current symbol in the bar
- shows parens completion upon print(
- does not show any code assist or code checking
- reopen dev.el to investigate more
- unclear on what this means ;; It is **STRONGLY** recommended that you use the base.el config if you want to use Eglot. Lots of completion things will work better.
- unclear what eglot or treesitter do - assume i know little about these
- go back to eglot section in dev.el
- confused if i need to add a python server program

- navigate to this, thoroughly confused by it - https://www.masteringemacs.org/article/seamlessly-merge-multiple-documentation-sources-eldoc

>> Eldoc is neither brassy nor is it intrusive, and it is why I love it. It epitomizes Emacs’s philosophy of quiet
>> enjoyment and distraction-free editing. It hides in the background, and it only emerges to share its thoughts about
>> the goings-on around point when it has something clever to say. Sadly, it’s often drowned out by its braying neighbors:
>> LSP notifications; compiler and linter messages from Flycheck or FlyMake and who knows what else.

  - I have no context on what eldoc, flycheck, flymake, lsp are.
  - where are each of the emacs features in their lifecycle? which are sunsetting? which are standard? which are emerging / experimental?
  - why would i want to display messages from "multiple documentation sources at the same time"? i just want one code assist system that works.

- how do i work with markdown?
  - how do i preview it?
  - i do a lot of math. how do i integrate latex, or something better than latex?
  - why doesn't my markdown show up correctly on github?

- i see no way to handle jupyter notebooks in emacs.
  - is there some notebook emulator? am i expected to set up a jupyter server? if so, i'll be editing in my browser. does emacs even matter in this use case?
  - how does this work with image packages like matplotlib?

- i read that eglot is a set of features for coding support - the emacs documentation shows it as almost a specification.
  - but what features within the spec are implmeneted for python? how do i know?
  - how does this differ from LSP, which also seems to offer a specification to implement?
  - how do eglot and LSP interelate if at all?

- i've spent two hours on this. i will pick this up again tomorrow and hope for the best. i have these things to read:
  - https://emacs-lsp.github.io/lsp-mode/
  - https://github.com/joaotavora/eglot
  - https://tree-sitter.github.io/tree-sitter/

- end scenario


## notes

- bedrock
  - oob config shows python file as this. what does WK and ElDoc mean? not clear to user
    - im.py Git:master  (Python WK ElDoc)
    - unclear how eldoc works or assists - is there inline documentation? 
  
  - error on startup: on oob startup: Symbol's function definition is void: tool-bar-mode

  - what are the external dependencies for python code support?

  - replace quick support with python developer specific keymaps

  - errors with eglot uncommented on python file open
    - Warning (treesit): Cannot activate tree-sitter, because language grammar for python is unavailable (not-found): (libtree-sitter-python.so libtree-sitter-python.so.0 libtree-si		tter-python.so.0.0 libtree-si\
      	      tter-python.dylib libtree-sitter-python.dylib.0 libtree-sitter-python.dylib.0.0) No such file or directory

  - dev.el file
  
    ;;; Usage: Append or require this file from init.el for some software
    ;;; development-focused packages.
    ;;;
    ;;; It is **STRONGLY** recommended that you use the base.el config if you want to
    ;;; use Eglot. Lots of completion things will work better.
    ;;;
    ;;; This will try to use tree-sitter modes for many languages. Please run
    ;;;
    ;;;   M-x treesit-install-language-grammar
    ;;;
    ;;; Before trying to use a treesit mode.



git clone https://codeberg.org/ashton314/emacs-bedrock.git
emacs --init-directory path/to/emacs-bedrock/

removing all pacakges to check for emacs dependencies
pip uninstall -r requirements.txt -y


### dependencies?

#### via brew
86074 s000  TN     0:00.09 /opt/homebrew/Cellar/python@3.12/3.12.7_1/Frameworks/Python.framework/Versions/3.12/Resources/Python.app/Contents/MacOS/Python /Users/gregf/.local/bin/pylsp

127] % brew list |grep py
python-lsp-server

#### via pip
pyflakes               2024-10-18  23:42:44                          3.2.0              pip             wheel   sys
tree-sitter            2024-10-18  22:59:55                          0.23.1             pip             sdist   sys



