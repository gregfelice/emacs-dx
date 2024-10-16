# emacs-dx

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [emacs-dx](#emacs-dx)
    - [Opportunity: Improve the Developer Experience in Emacs](#opportunity-improve-the-developer-experience-in-emacs)
    - [Problem Definition](#problem-definition)
    - [Characteristics of a Good Solution](#characteristics-of-a-good-solution)
    - [Design Considerations](#design-considerations)

<!-- markdown-toc end -->

## Opportunity: Improve the Developer Experience in Emacs

There's an opportunity to further help developers accelerate with Emacs by solving for some common (and solvable) problems in how tooling is constructed and shared on top of the base Emacs distribution.

I'm collecting perspectives in a document that I hope to share and refine with community members.

Would you mind meeting for 30 minutes? I'd love to ask you a few questions and understand your point of view on this subject.

## Vision

For software developers
That need to x, y, z
Our solution a
Does p, d, q
Unlike f, g
We r, t

### Personas

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

### User Stories

#### Goals


#### Complications
- I have to put hours or even days into a complete setup, and it often still doesn't work

### Root Problems
- TBD


### Metrics

- Developer

- Acquisition
    - # developers aware of solution
	- # developers downloading solution
    
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

### A Good Solution...

- a module is good because 
- fast
- simple
- maintained 
- Maintainable 
- good leader
- documented
- community
- easy to use 
- easy to find
- favored by core emacs dev, not deprecated 
- stable

### Design Constraints
- No backward compat
- codes to emacs 30 only
- no legacy el
- enforces testing
- enforces docs
- enforces publishing
- enables updates 
- has ci
- abandons external el in favor of 30 internal el

### Solution Features
- Jobs to do
- Outside in
- Contributor ecosystem 
- make it easy to know the rules for publishing 
- testing requirements
- documentation requirements

### Profiles
- Profile certification & incubation
- Runs on
- Uses
- Documentation
- Help channel
- Owner, committer
- Startup time
- Latency and performance 
- Update dunctions
- Diagnostic and inspection functions
- Bill of materials / manifests 
- What is it using that's outdated or deprecated 
- Integration testing between profiles
- Behaviour driven testing spec

### Workflow
 - Machine learning workflow
 - React workflow
 - Python workflow

# To Be Sorted

Init.el Auto config

Actions 
- clean up old and confusing communications
- define what is supported and what is not, and why

Architecture 
- automate all build config documentation publishing 
- make available on web

Information arch
- profile > goals
- workflow
- job to do / task
- tool

Components
- completion
- linter
- debugger
- style & syntax

Tasks
- project setup and config
- build 
- test
- package 
- deploy
- monitor and measure
- document
- version control
- build and ci status

Belief 
- all systems worth considering are text based 
- except for notebooks, plots, ui previews

Tests
- theme is readable
- no keymap clash 

Conflicting architecture areas
- eglot and lsp
- different modes for same language
- language combinations in same file 
- package loading
- conflicting themes

Profiles are opinionated 
Profiles combine jobs to do
Profiles are encapsulated
Profiles are authoritative for decisions on modes, etc

Projects
Combine profiles

Supports
- GitHub 
- react 
- Apache / data science
- spark
- phoenix envs to AWS, Google, MS 
- docker
- iterm2

- python
- JavaScript 
- typescript 
- jstx
- zsh

Most common and extensible
- tests 
- certifications
- publishing
- searching and matching















