---
theme: seriph
background: https://cover.sli.dev
Atitle: Learn Yuho in 5 minutes
info: |
  Yuho 
class: text-center
drawings:
  persist: false
transition: slide-left
mdc: true
---
# Learn how Yuho works in 5 minutes

---

## Introduction

![Legalese Confusion](./../asset/memes/canons_of_confusion.jpg)

Legalese is hard to understand for those unfamiliar with it. In fact, many even argue [legal jargon is a language unto itself](https://law.stackexchange.com/questions/95218/is-legalese-a-thing-in-languages-other-than-english). [Yuho](https://github.com/gongahkia/yuho) makes reading legalese easier to understand by reformatting and standardizing the [informal logic of the law](https://plato.stanford.edu/entries/logic-informal/) into the [formal logic of mathematics and computer science](https://plato.stanford.edu/entries/logic-classical/).

### Yuho is founded on the following beliefs:
1. Legalese is hard to understand
2. Textual explanations are good
3. Diagrammatic explanations are excellent

---

## An Example

Statutes aren't always intuitive.

![Monkey](./asset/monkey.jpg)

Below is Section 415 of the [Penal Code 1871](https://sso.agc.gov.sg/Act/PC1871) on the offense of Cheating in plaintext.

"Whoever, by deceiving any person, whether or not such deception was the sole or main inducement, fraudulently or dishonestly induces the person so deceived to deliver or cause the delivery of any property to any person, or to consent that any person shall retain any property, or intentionally induces the person so deceived to do or omit to do anything which he would not do or omit to do if he were not so deceived, and which act or omission causes or is likely to cause damage or harm to any person in body, mind, reputation or property, is said to cheat."


---

## Breaking Down the Statute

We can break the statute into its composite elements:

"Whoever, by deceiving any person,
WHETHER OR NOT such deception was the sole or main inducement,
fraudulently OR dishonestly induces the person so deceived
to deliver any property to any person,
OR to consent that any person shall retain any property,
OR intentionally induces the person so deceived
to do OR omit to do anything which he would not do
OR omit if he were not so deceived
AND which act or omission causes OR is likely
to cause damage OR harm to that person in body, mind, reputation, or property,
is said to cheat."


---

## Yuho Syntax Example

Once someone has learned the basics of Yuho's terse syntax, they can structure that same statute in Yuho as below:

scope s415CheatingDefinition {
struct Party { Accused, Victim }
struct AttributionType { SoleInducment, NotSoleInducement, NA }
struct DeceptionType { Fraudulently, Dishonestly, NA }
struct InducementType { DeliverProperty, ConsentRetainProperty, DoOrOmit, NA }
struct DamageHarmType { Body, Mind, Reputation, Property, NA }
struct ConsequenceDefinition { SaidToCheat, NotSaidToCheat }

struct Cheating {
    string || Party accused,
    string action,
    string || Party victim,
    AttributionType attribution,
    DeceptionType deception,
    InducementType inducement,
    boolean causesDamageHarm,
    {DamageHarmType} || DamageHarmType damageHarmResult,
    ConsequenceDefinition definition
}

Cheating cheatingDefinition := {
    accused := Party.Accused,
    action := "deceiving",
    victim := Party.Victim,
    attribution := AttributionType.SoleInducment or AttributionType.NotSoleInducement or AttributionType.NA,
    deception := DeceptionType.Fraudulently or DeceptionType.Dishonestly or DeceptionType.NA,
    inducement := InducementType.DeliverProperty or InducementType.ConsentRetainProperty or InducementType.DoOrOmit or InducementType.NA,
    causesDamageHarm := TRUE or FALSE,
    damageHarmResult := { DamageHarmType.Body, DamageHarmType.Mind, DamageHarmType.Reputation, DamageHarmType.Property } or DamageHarmType.NA,
    definition := match attribution {
        case AttributionType.SoleInducment := deception
        case AttributionType.NotSoleInducement := deception
        case AttributionType.NA := consequence ConsequenceDefinition.NotSaidToCheat
    },
}
}

---

## Diagrammatic Representations

This Yuho code can then be transpiled to various diagrammatic representations in [Mermaid](https://mermaid.js.org/).

### Mindmap Example

mindmap
Cheating
Accused: Party.Accused
Action: Deceiving
Victim: Party.Victim
Attribution
AttributionType.SoleInducment
AttributionType.NotSoleInducement
AttributionType.NA
Deception
DeceptionType.Fraudulently
DeceptionType.Dishonestly
DeceptionType.NA
Inducement
InducementType.DeliverProperty
InducementType.ConsentRetainProperty
InducementType.DoOrOmit
InducementType.NA
Causes Damage/Harm
TRUE
FALSE
Damage/Harm Result
DamageHarmType.Body
DamageHarmType.Mind
DamageHarmType.Reputation
DamageHarmType.Property
DamageHarmType.NA
Definition
ConsequenceDefinition.SaidToCheat
ConsequenceDefinition.NotSaidToCheat

---

## Where to Go Next?

* Learn Yuho's syntax at [`syntax.md`](./syntax.md)
* See other examples of Yuho at [`./example/`](./../../example/main/)
* Run Yuho's syntax through formal specifications with [Alloy Analyzer](https://alloytools.org/) at [`./tests/main`](./../../tests/main)
* Try Yuho out for yourself at [`./src/main/`](./../../src/main/)
* Build Yuho's lexer and parser yourself at [`./grammer/main/`](./../../grammer/main/)
* Have Racket, DSL development experience or want to gain that experience? See [`CONTRIBUTING.md`](./../../admin/CONTRIBUTING.md)
