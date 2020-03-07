
struct KindHashResolver {

}

impl KindHashResolver {
    fn tokenize(kind_hash: &KindHash) -> Vec<KindHashToken> {

    }

    fn parse(tokens: Vec<KindHashToken>) -> KindHashTree {
        let base = self.expect(CHUNK);
    }

    fn resolve_tree(kind_hash_tree: KindHashtree) -> KindHashTree {

    }
}

enum KindHashToken {
    Chunk(String),
    OpenDoubleAngleBracket,
    CloseDoubleAngleBracket,
    OpenCurlyBracket,
    CloseCurlyBracket,
    Comma,
}

struct KindHashTree {
    base: KindHash,
    params: Vec<KindHashTree>
}
