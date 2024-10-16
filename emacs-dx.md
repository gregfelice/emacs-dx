# emacs-dx

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [emacs-dx](#emacs-dx)
    - [Opportunity: Improve the Developer Experience in Emacs](#opportunity-improve-the-developer-experience-in-emacs)
    - [Vision](#vision)
    - [More Persona Details](#more-persona-details)
    - [User Stories](#user-stories)
        - [Metrics](#metrics)
        - [Potential Substitutes](#potential-substitutes)
        - [Acceptance Criteria](#acceptance-criteria)
    - [Solution Design](#solution-design)
        - [Containing Structures](#containing-structures)
            - [Roles & Journeys](#roles--journeys)
            - [Components (the modular architecture of all of this. design should align to the features and structures defined in the base emacs first. some terms here are just conceptual.)](#components-the-modular-architecture-of-all-of-this-design-should-align-to-the-features-and-structures-defined-in-the-base-emacs-first-some-terms-here-are-just-conceptual)
    - [Profiles](#profiles)
        - [Profile Bill of Materials (Included within all profiles)](#profile-bill-of-materials-included-within-all-profiles)
        - [Profile Catalog (Profiles offered for download and install)](#profile-catalog-profiles-offered-for-download-and-install)
    - [Distribution Channel](#distribution-channel)
        - [Website (github? other?)](#website-github-other)
- [To Be Sorted](#to-be-sorted)
- [Appendix](#appendix)
    - [-](#-)

<!-- markdown-toc end -->

## Opportunity: Improve the Developer Experience in Emacs

There's an opportunity to further help developers accelerate with Emacs by solving for some common (and solvable) problems in how tooling is constructed and shared on top of the base Emacs distribution.

I'm collecting perspectives in a document that I hope to share and refine with community members.

## Vision

**For software developers that have jobs to do**

**Our solution**
- Addresses common challenges developers experience when trying to get up to speed in Emacs
- Offers better tooling to support the higher level goals and objectives
- Educates along the way, but does not frontload learning requriements such that adopting Emacs becomes untentable

**Features include**
- Higher level modules based on role and popular tool combinations, e.g.,
  - React developer
  - AI / data science developer
  - GenAI developer
- Opinionated modules that make architectural decisions for the user
  - We use LSP - even though package X is "written in rust and faster", we are standardizing on this for maintainability
  - We make decisions on Eglot, LSP, flymake, etc etc so you don't have to
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
- VSCode (work on differentiation TBD)

**We will know when we are succeeding when we stop hearing...**
- When I need to combine tools to support my entire workflow, everyting starts breaking
- There's 1,000 helpful suggestions, and I can't get any working
- My configuration for jsp, ts, html, css, jsx, jstx is a mess, and never works as advertised
- There's ambiguous and conflicting recommendtations on core approaches for LSP vs Ruff. I don't understand them and I shouldn't have to
- Lots of information on the web is outdated. I wish there was a single source of information that helped me do my job

**We will know we are succeeding when we start hearing...**
- This is easier to set up and use than VSCode
- There's cool videos on YT that support my deeper understanding of this "jobs to do" oriented module scheme
- This supports everything I need
- I feel more integrated with the command line, and I like that
- I'm finally able to adopt emacs as my single go-to tool

## More Persona Details
- The Developer (Software, Data, Data Science, etc)
- Module Builders (Low level enabling, developer workflow enabling, etc)
- 3rd Party Software & Tooling Developers (Apache Spark, Beam, Scikit, etc)
- Emacs Governance & Maintainer Bodies (FSF, etc)
- Standards Governance Bodies (LSP, etc)
- Educators (YT, etc)

## User Stories
- TBD

### Metrics

- Developer

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


- Module Builder
  - Time to onboard module
  - Time to update module
  
- Emacs Governance & Maintainer Bodies
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

## Solution Design

### Containing Structures

#### Roles & Journeys

- Profile (see Doom Emacs)
- Workflow / Critical User Journey (CUJ) (somme definition of tasks within a user journey - e.g., setup & configuration, code, test, package, deploy, measure for an SDLC)
- Task (an individual job to do within the context of the Critical User Journey)
- Tool (something that is incorporated into the CUJ to enable a task

#### Components (the modular architecture of all of this. design should align to the features and structures defined in the base emacs first. some terms here are just conceptual.)

- Goal: Keep this simple. Don't create unneeded work. The value is in being able to provide guarantees this all just works OOB (out of the box).

- UI Enhancement
  - Themes (colors, etc)

- Coding Support
  - Completion
  - Linter
  - Debugger
  - Style & Syntax

- Documentation & Help (presented in the overall context of the "profile")
  - key combinations
  - task based context: to do this job, I can use these things
  - education (what the profile and module does, what underlying modules it uses, what architectural decisions were made)
  - everthing is well documented, and there's a single point of entry inside emacs for profile

- Configuration
  - module toggles (whatever is configurable is available in a sensible UI, and el is updated accordingly)
  - external dependency configurations (LSP, etc)
  - browser integration (matplotlib image outputs, local markdown preview, jupyter notebooks, etc)

- Project Management & Version Control
  - Project based configuations
  - Version control setup and management

- Build, Test, Package, Deploy
  - Command line based runs
  - Status of tooling integration (tools used, paths, folders, tool integration pass/fail)

- Terminal configuration
  - shell: zsh, bash, etc (don't force developers to learn eterm - they should not have to settle for anything less than a first-class command line experience)
  - terminal: works with iterm2, mac terminal, do not assume x-windows

- Infrastructure management
  - k8s and docker? 
  - Hooks into major cloud vendors?
  
- LLM integration
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

### Persona Detail

- The Developer
  - Software
    - Back End
	- Front End
  - Data
  - Data Science

- Module Builders

- Software & Tooling Developers
  - Apache Projects
    - Spark
	- Beam
  - AI Products & Library Maintainers
    - PYTorch
	- Tf
	- Numpy

- Emacs Governance & Maintainer Bodies

- Standards Governance Bodies
  - LSP

- Educators
  - YT
  - Lecturers
