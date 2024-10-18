
# emacs-dx

Document Status: DRAFT

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [emacs-dx](#emacs-dx)
    - [Solution Design Details](#solution-design-details)
        - [Containing Structures](#containing-structures)
            - [Roles & Journeys](#roles--journeys)
            - [Components ](#components)
    - [Profiles](#profiles)
        - [Profile Bill of Materials (Included within all profiles)](#profile-bill-of-materials-included-within-all-profiles)
        - [Profile Catalog (Profiles offered for download and install)](#profile-catalog-profiles-offered-for-download-and-install)
    - [Distribution Channel](#distribution-channel)
        - [Website (github? other?)](#website-github-other)
- [To Be Sorted](#to-be-sorted)
- [Appendix](#appendix)
    - [Customer Research](#customer-research)
        - [User Population Sizing](#user-population-sizing)
    - [Substitute Research](#substitute-research)
        - [VSCode](#vscode)
    - [Domain Research](#domain-research)

<!-- markdown-toc end -->


## Solution Design Details

### Containing Structures

#### Roles & Journeys

- Profile (see Doom Emacs)
- Workflow / Critical User Journey (CUJ) (somme definition of tasks within a user journey - e.g., setup & configuration, code, test, package, deploy, measure for an SDLC)
- Task (an individual job to do within the context of the Critical User Journey)
- Tool (something that is incorporated into the CUJ to enable a task

#### Components 

The modular architecture of all of this. design should align to the features and structures defined in the base emacs first. some terms here are just conceptual.

Goal: Keep this simple. Don't create unneeded work. The value is in being able to provide guarantees this all just works OOB (out of the box).

UI Enhancement
- Themes (colors, etc)

Coding Support
- Completion
- Linter
- Debugger
- Style & Syntax

Documentation & Help (presented in the overall context of the "profile")
- key combinations
- task based context: to do this job, I can use these things
- education (what the profile and module does, what underlying modules it uses, what architectural decisions were made)
- everthing is well documented, and there's a single point of entry inside emacs for profile

Configuration
- module toggles (whatever is configurable is available in a sensible UI, and el is updated accordingly)
- external dependency configurations (LSP, etc)
- browser integration (matplotlib image outputs, local markdown preview, jupyter notebooks, etc)

Project Management & Version Control
- Project based configuations
- Version control setup and management

Build, Test, Package, Deploy
- Command line based runs
- Status of tooling integration (tools used, paths, folders, tool integration pass/fail)

Terminal configuration
- shell: zsh, bash, etc (don't force developers to learn eterm - they should not have to settle for anything less than a first-class command line experience)
- terminal: works with iterm2, mac terminal, do not assume x-windows

Infrastructure management
- k8s and docker? 
- Hooks into major cloud vendors?

LLM integration
- OSS LLM version of copilot

## Profiles

### Profile Bill of Materials (Included within all profiles)

Automated Profile Tests
- Themes are readable
- No keymap clashes

### Profile Catalog (Profiles offered for download and install)

1. Global
  1. Version control: Github
  1. Terminal configuration: iterm2, mac terminal
  1. Shell configuration: zsh
  1. Universal color theme management

1. Profile: React.js development
  1. JavaScript 
  1. Typescript 
  1. jstx

1. Profile: Data science development
  1. Spark
  1. Python
  1. venv
  1. Anaconda

1. Profile: Infrastructure management
  1. AWS, Google, MS 
  1. Docker


## Distribution Channel
### Website (github? other?)
- Visible tests 
- Visible certifications
- Publishing
- Searching and matching

- Profile certification & incubation
- Runs on
- Uses
- Documentation
- Help channel
- Owner, committer
- Startup time
- Latency and performance 
- Update functions
- Diagnostic and inspection functions
- Bill of materials / manifests 
- What is it using that's outdated or deprecated 
- Integration testing between profiles
- Behaviour driven testing spec


# To Be Sorted
- 


# Appendix

## Customer Research

The Developer
- Software
  - Back End
  - Front End
- Data
- Data Science

Module Builders

Software & Tooling Developers
- Apache Projects
  - Spark
  - Beam
- AI Products & Library Maintainers
  - PYTorch
  - Tf
  - Numpy

Emacs Governance & Maintainer Bodies
- Standards Governance Bodies
  - LSP

Educators
- YT
- Lecturers

### User Population Sizing
These will be very wrong at first, but serve as a mental exercise to see what effort addresses what part of a potential user base, in what timeframe.

- React developers - 20 million?
- Data & AI developers - 30 million?
- Angular developers - 5 million?
- Scientific paper writers - ?

## Substitute Research

### VSCode
TBD

## Domain Research

Protesilaos Stavrou (Prot)
- https://www.youtube.com/@protesilaos
- https://protesilaos.com/

Henrik Lissner
- https://github.com/doomemacs

Nicolas Rougier
- https://github.com/rougier
- https://github.com/rougier/nano-emacs

Gavin Freeborn
- https://www.youtube.com/@GavinFreeborn


