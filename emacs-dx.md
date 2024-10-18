# emacs-dx

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [emacs-dx (The Emacs Developer Experience)](#emacs-dx-the-emacs-developer-experience)
    - [Developers Have Jobs to Do](#developers-have-jobs-to-do)
    - [Solution Approach & Principles](#solution-approach--principles)
    - [Critical Roles](#critical-roles)
        - [Metrics](#metrics)
        - [Acceptance Criteria](#acceptance-criteria)

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


