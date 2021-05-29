
---
title: MUltlog $ml_version$ & iLC $ilc_version$
---

MUltlog is a system which takes as input the specification of a
finitely-valued propositional or first-order logic and produces a
sequent calculus, tableau and natural deduction systems, and clause
formation rules for this logic. The output is in the form of a
scientific paper written in LaTeX.

The systems produced are the ones described by @BaazFermullerZach1994,
@BaazFermullerZach1993, @BaazFermullerZach1993b, @Zach1993. All
generated rules are optimized regarding their branching degree as
described by @Salzer2000. The system was first presented by
@Salzer1996a.

MUltlog is written in Prolog, and should run with any standard
interpreter (tested with SWI- and SICStus-Prolog). The input to
MUltlog can be created with any text editor. Viewing the generated
paper requires the LaTeX typesetting system.

iLC, the interactive Logic Creator, offers a graphical user interface
(written in a Tcl/Tk) for producing logic specification files for use
with MUltlog.

The experimental "interactive mode" lets you investigate many-valued
logics: find tautologies, check for entailment, define products and
factors of logics, or test for isomorphisms.

## Installation

MUltlog can be obtained from
[github.com/rzach/multlog](https://github.com/rzach/multlog).

For installation instructions, see the [MUltlog user
manual](multlog.html) (also available [in PDF](multlog.pdf)).

If you encounter problems, please [file an
issue](https://github.com/rzach/multlog/issues) on GitHub.

## Examples

A number of example of logic specifications are provided in the
[examples](https://github.com/rzach/multlog/tree/master/examples)
directory. They result in the following PDFs:

$for(example)$
  - [$example.name$]($example.link$)
$endfor$

## About MUltlog

MUltlog is a project by the Vienna Group for Multiple-valued Logics,
supported by FWF grant P10282-MAT (Austrian Science Foundation). The
people that contributed to MUltlog are (in alphabetical order):

- Stefan Katzenbeisser: rewrote (together with Stefan Kral) the
    optimization procedure for operators using more efficient data
    structures.
- Stefan Kral: see Stefan Katzenbeisser.
- Andreas Leitgeb:
    author of iLC (interactive Logic Creator), a window-oriented
    interface to MUltlog.
- Wolfram Nix: author of eLK, a menu-oriented
    DOS interface to MUltlog (no longer available).
- Alexandra Pascal: author of JMUltlog, a
    Java-based web interface to MUltlog (no longer available).
- [Gernot Salzer](https://www.logic.at/staffpages/salzer): author of the
    MUltlog kernel and coordinator of the project.
- Markus Schranz:
    author of the HTML/Perl-based web interface to MUltlog.
- [Richard Zach](https://richardzach.org):
    updated the LaTeX code for the 25 anniversary and included
    tableaux systems.

## References