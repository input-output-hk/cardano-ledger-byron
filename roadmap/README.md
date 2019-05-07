# Roadmap



## Byron-Shelley Bridge


### Finish work on Byron spec [1]

- [~~Federate protocol parameters in the ledger spec~~](https://github.com/input-output-hk/cardano-ledger-specs/issues/344)

- [~~Split protocol parameters into updatable and non-updateable~~](https://github.com/input-output-hk/cardano-ledger-specs/issues/343)

- [Model accounting identity reserves + sum utxo = constant (45b)](https://github.com/input-output-hk/cardano-ledger-specs/issues/299)

- [Specify the abstract size function on the abstract types](https://github.com/input-output-hk/cardano-ledger-specs/issues/298)

- [~~Switch to new notation for assignment/equality~~](https://github.com/input-output-hk/cardano-ledger-specs/issues/296)

- [~~Define the minimum amount of environment needed for the update interface rules~~](https://github.com/input-output-hk/cardano-ledger-specs/issues/285)

- [Adapt the formal spec to include metadata in the update](https://github.com/input-output-hk/cardano-ledger-specs/issues/271)

- [Document deviation in delegation validation](https://github.com/input-output-hk/cardano-ledger-specs/issues/244)


### Integrate Concrete Implementation with Consensus [1]

- ~~Change `updateChain` to reflect only the non-protocol parts~~
  - CLOSED: [2019-03-12 Tue 12:48]

- ~~Hook `updateChain` in to `applyLedgerState`~~
  - CLOSED: [2019-03-25 Mon 15:24]
  - Need to work out what to do with the genesis config
  - Convert from `MonadError` to `Except`.

- ~~Add `LedgerEnv` data family to `UpdateLedger`~~
  - CLOSED: [2019-03-25 Mon 15:23]
  - This will be used to deal with genesis config.

- ~~Add additional header processing method to `UpdateLedger`~~
  - CLOSED: [2019-03-25 Mon 15:24]

- Split between header and ledger state
  - At the moment, these are treated as a single state type continuously updated.
    Aim is to be able to compute header state for slots in a window around the
    current slot.

    Also want to be able to step the header state as we go forward.

    Window denotes the period in which we can step forward. When we decide on an
    actual fork, we run forward and re-incorporate the changes (e.g. we don't try
    to re-fold in the header state).

    Bracket the header and chain updates in the `applyLedgerStateExt`.

    `step :: LedgerState -> Header -> HeaderState -> Either Error HeaderState`

    Note that this depends on the header/body split being introduced - Thomas
    Winant is working on this.

- ~~Implement PBFT protocol~~
  - CLOSED: [2019-03-29 Fri 14:17]

- ~~Test PBFT against mock ledger~~

- Test PBFT against real (Byron) ledger


### Validate Valid Generated Chains against Full Spec [2]

- Byron ledger rules fully implemented in executable spec - Nick/Luke
  - ~~Delegation rules~~
  - UTXO rules
    - Incorporate update rules
  - ~~Update rules~~
  - ~~Update interface rules~~
  - Chain rule tying everything together

- ~~Byron ledger rules fully implemented in implementation [Milestone?]~~
  - ~~Delegation rules~~
  - ~~UTXO rules~~
    - ~~Incorporate update rules~~
  - ~~Update rules~~
  - ~~Update interface rules~~
  - ~~Chain rule tying everything together~~

- ~~Test validation rules against mainnet~~
  - ~~Add update mechanism into mainnet chain tests~~

- Generator for (valid) full chain
  - ~~Merge Ru’s PR with hedgehog stuff~~
  - Implement generator for blocks with all stuff
  - ~~Scaffolding to embed CHAIN inside h-s-m~~
  - ~~Scaffolding for other systems inside h-s-m~~

- Elaboration of full abstract chain

- Remove ‘HasTrace’
  - Change existing properties to use hedgehog state machine approach

- ~~Test harness to generate chains, elaborate, and validate them~~

- Account principle (nice-to-have)
  - Design in LaTeX spec
  - Implement in executable spec
  - Implement in concrete rules


### Finish work on Byron executable specs [2]

- [Implement PBFT rules and any missing dependency STSes in Haskell](https://github.com/input-output-hk/cardano-ledger-specs/issues/364)

- [~~Replace Map with Bimap in the delegation state (PR #400)~~](https://github.com/input-output-hk/cardano-ledger-specs/issues/326)

- [Add coverage information in the delegation spec tests](https://github.com/input-output-hk/cardano-ledger-specs/issues/297)

- [Check property: every valid protocol update allows to produce new blocks](https://github.com/input-output-hk/cardano-ledger-specs/issues/287)

- [Add property test: utxo and transaction outputs must be disjoint](https://github.com/input-output-hk/cardano-ledger-specs/issues/284)

- [Identify the transition systems that need properties and write issues for it](https://github.com/input-output-hk/cardano-ledger-specs/issues/268)

- [~~Shrinking takes an enormous amount of time~~](https://github.com/input-output-hk/cardano-ledger-specs/issues/267)

- [~~Bring the chain executable spec in sync with the current chain spec~~](https://github.com/input-output-hk/cardano-ledger-specs/issues/265)

- [~~Remove TODO's from chain and ledger spec~~](https://github.com/input-output-hk/cardano-ledger-specs/issues/264)

- [Add property tests for UTxO: no double spending and utxo is inputs minus outputs](https://github.com/input-output-hk/cardano-ledger-specs/issues/263)

- [~~Define a semigroup instance for `PairSet` (PR #412)~~](https://github.com/input-output-hk/cardano-ledger-specs/issues/262)


### Audit Executable Spec against Formal Spec [3]

We should wait until the executable spec is fully implemented to do a full audit


### Live Chain Validation in `cardano-shell` [3]

- Live validation [Break down?]
  - Byron-proxy, responsible for downloading blocks (Alexander Vieth needs to complete this) - https://github.com/input-output-hk/ouroboros-network/pull/410
  - Familiarize with the proxy API
  - Create live chain validation application


### Valid Invalid Generated Chains [3]

- Audit goblins work to see if we can use it (watch technical meeting presentation and talk to Nick)

- Create a plan for working on this and including it in the tests


### Performance Optimisation

- Profile chain validation and identify performance hot-spots
  - Audit memory-use during a bulk validation (ensure there aren’t any leaks)
  - Ensure that the ledger state does not contain any thunks at each step after applying state update rules


### Address Technical Debt

- [Give `SlotId` a more specific name, and `FlatSlotId` a better name](https://github.com/input-output-hk/cardano-ledger/issues/170)

- [~~Investigate epochSlots size~~](https://github.com/input-output-hk/cardano-ledger/issues/209)

- [Identify for which modules we could add doctests and create tasks for doing this](https://github.com/input-output-hk/cardano-ledger/issues/331)

- [The types in kEpochSlots are misleading](https://github.com/input-output-hk/cardano-ledger/issues/333)

- [~~Propagate Bi to From/ToCBOR Change~~](https://github.com/input-output-hk/cardano-ledger/issues/145)
