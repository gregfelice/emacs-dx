# emacs-dx (The Emacs Developer Experience)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [emacs-dx (The Emacs Developer Experience)](#emacs-dx-the-emacs-developer-experience)
    - [Developers Have Jobs to Do](#developers-have-jobs-to-do)
    - [Critical Roles](#critical-roles)
    - [User Stories](#user-stories)
        - [Metrics](#metrics)
        - [Potential Substitutes](#potential-substitutes)
        - [Acceptance Criteria](#acceptance-criteria)
    - [Solution Design](#solution-design)
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

## Developers Have Jobs to Do

**Many developers want to use Emacs, but quickly give up on it because of a frustrating and time consuming configuration experience. The root problems driving these frustrations are solvable. This document describes how to help developers quickly get productive on Emacs on the tasks that matter to them. We believe we can increase the number of developers who try Emacs, get value quickly enough to use it more, and promote its virtues to friends.**

**For software developers**
- That have deadlines and priorities, and often make their living coding
- Who need to make economic decisions about their scarce time
- Who love tooling and learning deeply, but can't frontload days of work to get it working
- Who may feel passion and affection for open source and Emacs, but sadly abandon it because it overfocuses on details without addressing their larger professional need

**Our solution**
- Addresses common challenges developers experience when trying to get up to speed in Emacs
- Reduces friction on finding, trying, using, and managing Emacs based tools
- Provides constructs that enable management of groups of pacakges that must interoperate harmoniously to be effective
- Focuses on better tooling for in-demand roles like react development, AI & data science development or full-stack combinations of both
- Offers clear comments and guidance on **what** certain elisp configurations do, and **why** a certain pacakge or standard was chosen over another
- Ensures the effort to configure Emacs does not outweigh the benefits of using it

**Features include**
- Higher level modules based on role and popular tool combinations, e.g.,
  - React developer
  - AI / data science developer
  - GenAI developer
- Opinionated modules that make architectural decisions for the user
  - e.g., We use LSP - even though package X is "written in rust and faster", we are standardizing on this for maintainability
  - e.g., We make decisions on Eglot, LSP, flymake, etc etc so you don't have to
- Simplicity and reliability
  - Modules certification that ensures they are tested and working
  - Modules that support only 29 and 30
  - Elimination of complex and hard to maintain elisp to acommodate thousands of modules and configuration edge cases
- Module certification framework
  - Makes it easy for module developers to design to a specification
  - Guidance and structure for test design and continuous integration
- Module publishing
  - One place to find certified modules
  - Web analytics to measure adoption
  - Issue reporting back to module developers
  - Lightweight & transparent governance

**Unlike substitutes**
- VSCode
  - GUI driven. Does a really nice job at things, but ultimately not oriented to the terminal oriented developer
- Emacs Distros
  - Admirable efforts to tame the configuration complexity of Emacs, but still have problems meeting the specific needs of in-demand development roles
  - Often do too much at the expense of any single real world professional usage scenario
  - Also introduces "higher order" organizational constructs such as profiles, but they are not yet utilized. Is there an opportunity to leverage / help accelerate?

**We will know when we are succeeding when we stop hearing...**
- When I need to combine tools to support my entire workflow, everyting starts breaking
- There's 1,000 helpful suggestions, most are incomplete, and I can't get anything working
- My configuration for jsp, ts, html, css, jsx, jstx is a mess, and never works as advertised
- There's ambiguous and conflicting recommendtations on critical setup approaches (e.g., language servers command completion, etc). I don't understand them **and I shouldn't have to**
- Lots of information on the web is outdated. I wish there was a single source of information that helped me do my job
- I have a day job and don't have time for this

**We will know we are succeeding when we start hearing...**
- This is easier to set up and use than VSCode
- There's cool videos on YT that support my deeper understanding of this "jobs to do" oriented module scheme
- This supports everything I need
- I feel more integrated with the command line, and I like that
- I'm finally able to adopt Emacs as my go-to tool

## Solution Approach & Principles
- Recognize different roles with different goals: 
  - Software developers who want to use Emacs for the best text and command line based development experience
  - Module builders who love the extensibility of Emacs
- Recognize the need to build modules for a user in mind - in this case, a software developer who can choose whatever they want
- Address the problem of cross-package interdependencies
  - This is an area where little vaidation seems to occur, and it drives a great deal of overly complex configurations and causes errors
- Believe that while Emacs may be infinitely extensible, it needs to take specific forms to drive value at any scale
- Believe that there is value in combining efforts to get a developer profile right to potentially solve problems for a million users
- Regardless of specific role, the general target user is a power user that loves command lines, source control, and terminals. This can be a web developer, data scientist, or infrastructure engineer

## Critical Roles
We'll end up needing to consider the critical jobs for these roles. 
- The Developer (Software, Data, Data Science, etc)
- Module Builders (Low level enabling, developer workflow enabling, etc)
- 3rd Party Software & Tooling Developers (Apache Spark, Beam, Scikit, etc)
- Emacs Governance & Maintainer Bodies (FSF, etc)
- Standards Governance Bodies (LSP, etc)
- Educators (YT, etc)

## User Stories
We can detail specific tasks to perform here. Some examples:

- As a react.js developer, I can download a profile that integrates all my build tooling, various source code tooling, etc. knowing it will work out of the box
- As a react.js developer, I will not need to immediately know the differences and overlaps between jstx-module, web-module, etc. Choices will be made for me, and it will just work
- As a react.js developer, I can easily enable next.js support, tusting the react profile packagers understand the most popular react extensions and how to support them.

- As a full stack developer, I can configure two profiles side by side and know they harmonize, such as data science using spark, and react js.

- As a zsh user, the profile I download will give me a no-compromise zsh command line experience, with no confusion on environment variables, in Emacs shells vs outside shells, etc.

- As a developer, I want to configure my fonts to my tastes without breaking anything, so I can have an enjoyable coding experience day and night.

- As a developer, I want to eliminate any unused modules from my searches, so I can eliminate cognitive overload.

- As a developer, I want to ensure all the features defined in a language server spec like LSP are working, so I can gt down to coding.
- As a developer, I want to eliminate any guesswork from what modules are considered "current best practice", so I can get down to coding.
- As a developer, I want less "endless choice" and "extensibility", and more "usability" and "productivity", so I can get my job done. 
- As a developer, I expect Emacs to integrate into best practice code > build > test workflows for my tribe, like React. The Emacs tail should not wag the dog.


### Metrics

Developer
- Acquisition
  - num of developers aware of solution
  - num of developers downloading solution
  
- Activation
  - Time to hello world in language & workflow of choice

- Retention
  - Engagement (MAU / DAU)

- Monetization
  - Patreon donations to support FSF / Emacs development
  - YT engagaement for community contributors
  - Monthly Active Users (MAU)
  - Daily Active Users (DAU)

Module Builder
- Time to onboard module
- Time to update module

Emacs Governance & Maintainer Bodies
- Emacs user metrics

### Potential Substitutes

- VSCode

### Acceptance Criteria
- Fast
- Simple
- Maintained and maintainable 
- Good, visible and accessible leader
- Documented
- Community
- Easy to use 
- Easy to find
- Favored by core emacs dev, not deprecated 
- Stable
- No backward compatibility code, codes to emacs 29 & 30 only

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




