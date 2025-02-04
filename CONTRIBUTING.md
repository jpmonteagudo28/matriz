
<!-- Taken and modified from https://github.com/mbutterick/pollen/tree/master -->

## Pull-request tips

I welcome pull requests. But accepting a PR forces me to maintain that
code for the life of `matriz`. So if I seem picky about which PRs I
accept — yes, because I have to be. No hard feelings. (= Principle of
Infinite Maintenance)

- There’s **plenty** of room for improvement in the `matriz` code,
  because every line of it has been written against the backdrop of
  ignorance and fallibility, mostly my own. (= Principle of Prior
  Ignorance)

- PRs for simple documentation fixes (e.g., spelling and grammar
  corrections) are always welcome. For more substantial changes, I don’t
  necessarily prefer PRs to issues or feature requests. A good
  description of the problem with a working example is better than a
  half-baked PR. I can often fix it in less time than it would take to
  review the PR. (= Principle of Efficiency)

- If you want feedback on a potential PR, I recommend opening an
  [issue](https://github.com/jpmonteagudo28/matriz/issues) rather than
  reaching out to me directly. Because I or someone else will see it. (=
  Principle of Exposure)

- Small PRs are easier to accept than large ones. Large PRs should have
  a benefit worthy of their complexity. PRs that want to amend the
  package’s external functions receive the highest scrutiny. (=
  Principle of Proportionality)

- I consider every PR, but I can’t promise detailed code reviews or
  comments. Helpful R users can be found on the [Posit Community
  forum](https://forum.posit.co/), the [R Language
  Collective](https://stackoverflow.com/collectives/r-language), and the
  [Data Science Learning Community’s slack channel](https://dslc.io/).
  (= Principle of Specialization)

- PRs should be necessary, in the sense that the proposed change can
  only be accomplished by patching this repo. (= Principle of Necessity)

- PRs should avoid introducing magic behavior (= [Principle of Least
  Astonishment](http://wiki.c2.com/?PrincipleOfLeastAstonishment)).

- PRs should forbid as little as possible. In particular, PRs should
  avoid enshrining personal preference as default behavior (because
  others will have different preferences). (= Principle of Generality)

- PRs should avoid reinventing features that already exist in R. (=
  Principle of Economy)

- PRs should fix real problems that have arisen in actual use, not
  theoretical or conjectural problems. (= Principle of Practical
  Justification)

- I follow these principles too, because they’re virtuous habits. Still,
  I created `matriz` as a tool for my writing and research. If a certain
  PR would negatively impact that work, I can’t accept it. (= Principle
  of Royalty)

- If you’re new to R, your PR is more likely to be declined, because
  certain things you perceive as bugs are actually features, certain
  things you perceive as missing are actually present, and certain
  limitations you perceive as surmountable are actually not. (See also
  point \#1 re: backdrop of ignorance.) (= Principle of Novelty)

- If your PR includes open-source material from elsewhere, please make
  sure that material is a) compatible with the `matriz` license and b)
  attributed in whatever way is required. Otherwise, I cannot accept it.
  (= Principle of Legality)

- PRs that could have unit tests, and don’t, will be treated harshly. As
  they should. (= Principle of Proof)
