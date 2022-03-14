open util/relation as rel

enum Bool { False, True }
sig ID {}
sig CID {}
sig Name {}
sig URL {}
sig Type {}

sig Entry {
    /* These are functions */
    hasCID : one CID,
    ofType : one Type,
    isNamed : one Name,
    consumed : one Bool,

    /* This one is functional (i.e., partial function) */
    HasURL : lone URL,
}

some sig Node {
    identifiedBy : one ID,
    Pinned : set CID
}


pred NoFreeAttributes {
    CID = Entry.hasCID
    URL = Entry.HasURL
    Type = Entry.ofType
    Name = Entry.isNamed

    ID = Node.identifiedBy
}

pred EntryMultiplicities {
    rel/bijection[hasCID, Entry, CID]
    rel/surjective[isNamed, Name]
    rel/surjective[ofType, Type]
    rel/bijective[HasURL, URL]
    rel/surjective[consumed, Bool]
}

pred NodeMultiplicities {
    rel/bijection[identifiedBy, Node, ID]
}

pred UnconsumedEntriesArePinned {
    //all e:Entry | e.consumed = False => e.hasCID in Node.Pinned
    False.~consumed.hasCID in Node.Pinned
}

fact {
    NoFreeAttributes
    EntryMultiplicities
    NodeMultiplicities
    UnconsumedEntriesArePinned
}

run {
    #Entry > 1
    #Node > 1
}
