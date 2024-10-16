# emacs-dx

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [emacs-dx](#emacs-dx)
    - [Developers Have Jobs to Do](#developers-have-jobs-to-do)
    - [Guiding Principles](#guiding-principles)
    - [Acceptance Criteria](#acceptance-criteria)
    - [Related Documents](#related-documents)

<!-- markdown-toc end -->


## Developers Have Jobs to Do

**Many developers want to use Emacs, but quickly give up on it because of a frustrating and time consuming configuration experience. The root problems driving these frustrations are solvable. This document is a draft vision for how to help developers quickly get productive on Emacs on the tasks that matter to them. I believe we can increase the number of developers who try Emacs, get value quickly enough to use it more, and promote its virtues to friends.**

**This document is an imperfect draft, written with the intention to collaborate with others to define a realistic and achievable plan, and launch a first iteration in a reasonable timeframe. Reach out if you're interested.**

[emacs-dx Discussion on Discord](https://discord.gg/9F4DSE25r7)

**Minimum Viable Design**
- Goal - get a developer productive in under 10 minutes - asumes familiarity with base emacs
- Support 2-3 Developer Profiles with curated and reliable el, available for clone on github, supported against vanilla emacs
  - profile-python-developer.el (release 1)
  - profile-react-developver.el (release 2)
  - profile-infrastructure-manager.el (release 3)
- User can activate one or many profiles without conflicts
  - keymappings are relevant and all work, no superfluous commands in search command completion (no js when im in python, only one prettify package, etc)
  - all developer features work without additional configuration: at-point documentation, on-the-fly-diagnostic annotation, definitions and identifiers, buffer navigation by symbol, completion at-point, code insertion, markdown
  - all document types required for role work properly (e.g., js, ts, jsx, jstx, mixed mode html/css)
- A simple profile-base.el package provides sensible comforts that all profiles inherit (themes, search enhancements, etc)
- Maintainers collaborate on architectural decisions that affect all packages (How to approach LSP, eglot, flymake, etc)
- Pull requests are accepted from contributors


# Proposed Vision

**For software developers**
- That have deadlines and priorities, and often make their living coding
- Who need to make economic decisions about their scarce time
- Who prefer to work at command lines, often move between multiple machines handling infrastructue and coding tasks, and need a powerful editor to get jobs done
- Who love tooling and learning deeply, but can't frontload days of work to get it working
- Who may feel passion and affection for open source and Emacs, but sadly abandon it because it does not meet their professional needs

**A good solution...**
- Ensures the effort to configure Emacs does not outweigh the benefits of using it
- Addresses common challenges developers experience when trying to get up to speed in Emacs
- Enables better management of package groups that must interoperate harmoniously to be effective
- Narrows the focus on better tooling for in-demand roles like react development, AI & data science development or full-stack combinations of both
- Offers prescriptive guidance on standards and architectural choices, explains what options were considered, and why one was ultimately selected

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
- This supports a lot of what I need to be productive, and harmonizes with key tools that don't make sense in the editor
- I feel more integrated with the command line, and I like that
- I'm finally able to adopt Emacs as a go-to tool

## Guiding Principles
- Agree that while Emacs may be infinitely extensible, it needs to take specific forms to drive value at any scale.
- While there are different flavors of Emacs users, take the perspective that the developer is an important customer, and more effort should be made to understand and address their needs.
- Identify the right collaboration strucrures to better govern Emacs to solve for developer problems. Create these structures if they don't exist.
- Look to the community for solutions. Distros like Doom Emacs identify these same problems, and even have design structures that can help accelerate emacs-dx objectives. See what can be leveraged without reinventing the wheel. Give credit to the amazing contributions of the distro maintainers who've been working this problem in various forms for years.
- Recognize the problems developers have in adopting and using Emacs. Have a desire for solving them.
- Recognize that nobody can do it all. Emacs-dx will need to focus on 1-2 developer types with large user bases (for example, React.js, Python) to ship anything in a reasonable timeframe.
- Recognize the importance of module developers, but agree on their responsibility to developers as the Emacs customer if they want to collaborate on emacs-dx.
- Recognize the importance of the part-time contributor community. Help them contribute improvements with solid frameworks, well-documented design constraints, and knowledgable governance to drive excellence.
- Recognize the importance of the Emacs codebase developers. Seek their advice on where the overall architecture for Emacs is going. Consider this when designing modules.

## Acceptance Criteria
- Fast
- Simple
- Maintained and maintainable 
- Good, visible and accessible leader
- Documented
- Active community
- Easy to use 
- Easy to find
- Favored by core emacs dev, not deprecated 
- Stable

## Related Documents
* [Detailed Requirements](emacs-dx-user-stories.md)
* [Design and Implementation Plan](emacs-dx-plan.md)


