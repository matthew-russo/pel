use crate::utils::utils;
use crate::evaluator::evaluator::{
    Heap,
    Kind,
    KindHash,
    KindTable,
};

struct KindHashTokenizer<'a> {
    kind_hash: &'a KindHash,
    tokens: Vec<KindHashToken>,
    current: usize
}

impl <'a> KindHashTokenizer<'a> {
    fn of(kind_hash: &'a KindHash) -> KindHashTokenizer<'a> {
        KindHashTokenizer {
            kind_hash,
            tokens: Vec::new(),
            current: 0
        }
    }

    fn next(&self) -> char {
        self.kind_hash.chars().nth(self.current).unwrap()
    }

    fn double_next(&self) -> Option<char> {
        self.kind_hash.chars().nth(self.current + 1)
    }

    fn chomp(&mut self) -> char {
        let next = self.kind_hash.chars().nth(self.current).unwrap();
        self.current += 1;
        next
    }
    
    fn tokenize(&mut self) -> Vec<KindHashToken> {
        let mut tokens = Vec::new();
        let mut curr = Vec::new();

        loop {
            if self.current >= self.kind_hash.len() {
                if curr.len() > 0 {
                    tokens.push(KindHashToken::Name(curr.iter().collect()));
                }
                break;
            }

            if '<' == self.next()
                    && self.double_next().is_some()
                    && '<' == self.double_next().unwrap() {
                if curr.len() > 0 {
                    tokens.push(KindHashToken::Name(curr.iter().collect()));
                    curr = Vec::new();
                }
                self.chomp();
                self.chomp();
                tokens.push(KindHashToken::OpenDoubleAngleBracket);
                continue;
            }

            if '>' == self.next()
                    && self.double_next().is_some()
                    && '>' == self.double_next().unwrap() {
                if curr.len() > 0 {
                    tokens.push(KindHashToken::Name(curr.iter().collect()));
                    curr = Vec::new();
                }
                self.chomp();
                self.chomp();
                tokens.push(KindHashToken::CloseDoubleAngleBracket);
                continue;
            }

            if '{' == self.next() {
                if curr.len() > 0 {
                    tokens.push(KindHashToken::Name(curr.iter().collect()));
                    curr = Vec::new();
                }
                self.chomp();
                tokens.push(KindHashToken::OpenCurlyBracket);
                continue;
            }

            if '}' == self.next() {
                if curr.len() > 0 {
                    tokens.push(KindHashToken::Name(curr.iter().collect()));
                    curr = Vec::new();
                }
                self.chomp();
                tokens.push(KindHashToken::CloseCurlyBracket);
                continue;
            }

            if ',' == self.next() {
                if curr.len() > 0 {
                    tokens.push(KindHashToken::Name(curr.iter().collect()));
                    curr = Vec::new();
                }
                self.chomp();
                tokens.push(KindHashToken::Comma);
                continue;
            }

            if self.next().is_whitespace() {
                if curr.len() > 0 {
                    tokens.push(KindHashToken::Name(curr.iter().collect()));
                    curr = Vec::new();
                }
                self.chomp();
                continue;
            }

            curr.push(self.chomp());
        }
        
        tokens
    }
}

struct KindHashParser {
    tokens: Vec<KindHashToken>,
    current: usize,
}

const NAME: KindHashToken = KindHashToken::Name(String::new());
impl KindHashParser {
    pub fn of(tokens: Vec<KindHashToken>) -> Self {
        Self {
            tokens,
            current: 0,
        }
    }

    pub fn parse(&mut self) -> KindHashTree {
        self.kind_hash_tree().unwrap()
    }

    fn current_token(&self) -> KindHashToken{
        self.tokens[self.current].clone()
    }

    fn expect(&mut self, expected_token: KindHashToken) -> Option<KindHashToken> {
        if self.current >= self.tokens.len() {
            return None;
        }

        let next_token = self.current_token();

        if utils::discriminants_equal(&expected_token, &next_token) {
            self.current = self.current + 1;
            return Some(next_token);
        }

        None
    }

    fn extract_name(t: &KindHashToken) -> Option<String> {
        // TODO -> maybe get rid of clone
        return match t {
            KindHashToken::Name(n) => Some(n.clone()),
            _ => None
        }
    }

    fn kind_hash_tree(&mut self) -> Option<KindHashTree> {
        let ty = self.ty()?;
        let params = self.type_params()?;
        Some(KindHashTree {
            ty,
            params,
        })
    }

    fn ty(&mut self) -> Option<Type> {
        let name_token = self.expect(NAME)?;
        let kind_hash = Self::extract_name(&name_token).unwrap();
        let type_param = if let Some(_) = self.expect(KindHashToken::OpenCurlyBracket) {
            let name_token = self.expect(NAME)?;
            let type_param_name = Self::extract_name(&name_token).unwrap();
                
            self.expect(KindHashToken::CloseCurlyBracket);
            Some(type_param_name)
        } else {
            None
        };

        Some(Type {
            kind_hash,
            type_param,
        })
    }

    fn type_params(&mut self) -> Option<Vec<KindHashTree>> {
        let mut params = Vec::new();

        if let Some(_)  = self.expect(KindHashToken::OpenDoubleAngleBracket) {
            params.push(self.kind_hash_tree()?);

            loop {
                if let Some(_) = self.expect(KindHashToken::Comma) {
                    params.push(self.kind_hash_tree()?) 
                } else {
                    break;
                }
            }

            self.expect(KindHashToken::CloseDoubleAngleBracket)?;
        }

        Some(params)
    }

}

fn resolve_tree(kind_hash_tree: KindHashTree, kind_table: &KindTable, heap: &Heap) -> KindHash {
    // let kind_hash = if let Some(param_name) = kind_hash_tree.ty.type_param {
    //     kind_table.keys().iter().for_each(|kh| println!("\t kh: {:?}", kh));
    //     println!("HUYHUY: {:?}", kind_hash_tree.ty.kind_hash);
    //     let ty_ref: Reference = match kind_table.load(&kind_hash_tree.ty.kind_hash).unwrap() {
    //         // Kind::Object(o_arc) => o_arc.read().unwrap().environment.read().unwrap().get_reference_by_name(&param_name).unwrap(),
    //         // Kind::Enum(e_arc) => e_arc.read().unwrap().environment.read().unwrap().get_reference_by_name(&param_name).unwrap(),
    //         // Kind::FunctionSignature(fs_arc) => fs_arc.read().unwrap().environment.read().unwrap().get_reference_by_name(&param_name).unwrap(),
    //         k => panic!("kind cannot be parameterized by types: {:?}", k),
    //     };
    //     heap.load_type_reference(ty_ref.to_heap_ref().unwrap().address).unwrap()
    // } else {
    //     kind_hash_tree.ty.kind_hash
    // };

    let kind_hash = kind_hash_tree.ty.kind_hash;
   
    let params: Vec<KindHash> = kind_hash_tree
        .params
        .into_iter()
        .map(|p| resolve_tree(p, kind_table, heap))
        .collect();

    if !params.is_empty() {
        let joined = params.join(",");
        KindHash::from(format!("{}<<{}>>", kind_hash, joined))
    } else {
        kind_hash
    }
}

pub(crate) fn resolve_kind_hash(kind_hash: &KindHash, kind_table: &KindTable, heap: &Heap) -> KindHash {
    let tokens = KindHashTokenizer::of(kind_hash).tokenize();
    let tree = KindHashParser::of(tokens).parse();
    resolve_tree(tree, kind_table, heap)
}

#[derive(Debug, Clone)]
enum KindHashToken {
    Name(String),
    OpenDoubleAngleBracket,
    CloseDoubleAngleBracket,
    OpenCurlyBracket,
    CloseCurlyBracket,
    Comma,
}

struct Type {
    kind_hash: KindHash,
    type_param: Option<String>,
}

struct KindHashTree {
    ty: Type,
    params: Vec<KindHashTree>
}
