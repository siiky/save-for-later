open util/relation as rel

enum Bool { False, True }
sig ID {}
sig CID {}
sig EntryName {}
sig NodeName {}
sig URL {}
sig Type {}

sig Entry {
    /* These are functions */
    hasCID : one CID,
    ofType : one Type,
    isNamedEntry : one EntryName,
    consumed : one Bool,

    /* This one is functional (i.e., partial function) */
    HasURL : lone URL,
}

some sig Node {
    identifiedBy : one ID,
    isNamedNode : lone NodeName,
    Pinned : set CID
}


pred NoFreeAttributes {
    CID = Entry.hasCID
    URL = Entry.HasURL
    Type = Entry.ofType
    EntryName = Entry.isNamedEntry

    ID = Node.identifiedBy
    NodeName = Node.isNamedNode
}

pred Multiplicities {
    /* Entry multiplicities */
    rel/bijection[hasCID, Entry, CID]
    rel/surjective[isNamedEntry, EntryName]
    rel/surjective[ofType, Type]
    rel/bijective[HasURL, URL]
    rel/surjective[consumed, Bool]

    /* Node multiplicities */
    rel/bijection[identifiedBy, Node, ID]
    rel/bijective[isNamedNode, NodeName]
}

pred UnconsumedEntriesArePinned {
    //all e:Entry | e.consumed = False => e.hasCID in Node.Pinned
    False.~consumed.hasCID in Node.Pinned
}

fact {
    NoFreeAttributes
    Multiplicities
    UnconsumedEntriesArePinned
}

run {
    #Entry > 1
    #Node > 1
}
