# Haskdaq

Haskdaq is a parser for _Event Storage_ and _E-format_ file formats used by the
ATLAS high-energy physics experiment, implemented in Haskell on top of
Attoparsec.

## Design

The design of Haskdaq is modular, to model the modular nature of _Event
Storage_. The event storage parser takes a sub-parser which determines the
structure of the internal data. Currently, only _E-format_ is supported.
_E-format_ is supported both in compressed an uncompressed format.

## Building

Haskdaq can be build by executing the following command:

```bash
$ stack build
```

The default executable takes a single argument, which must be a binary file in
_Event Storage_ format containing, in turn, _E-format_ events. For example:

```bash
$ stack run data22_13p6TeV.00431885.physics_EnhancedBias.merge.RAW._lb0545._SFO-15._0001.1
```

## Documentation

The following (CERN-internal) documents are available as documentation

* Specification of the _Event Storage_ format:
  https://edms.cern.ch/ui/file/580290/7/DC-066.pdf
* Specification of the _E-format_ format:
  https://edms.cern.ch/ui/file/445840/5.0.3/eformat503.pdf
