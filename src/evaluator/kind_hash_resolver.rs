use std::sync::{Arc,RwLock};

use crate::utils::utils;
use crate::evaluator::evaluator::{
    Environment,
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
        if let Some(name_token) = self.expect(NAME) {
            let kind_hash = Self::extract_name(&name_token).unwrap();
            Some(Type::KindHash(kind_hash))
        } else if let Some(_) = self.expect(KindHashToken::OpenCurlyBracket) {
            let name_token = self.expect(NAME)?;
            let type_param_name = Self::extract_name(&name_token).unwrap();
            self.expect(KindHashToken::CloseCurlyBracket).unwrap();
            Some(Type::TypeParam(type_param_name))
        } else {
            None
        }
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

fn resolve_tree(kind_hash_tree: KindHashTree, environment: &Arc<RwLock<Environment>>, heap: &Heap) -> KindHash {
    let base = match kind_hash_tree.ty {
        Type::KindHash(kh) => kh,
        Type::TypeParam(type_param_name) => {
            let type_ref = environment.read().unwrap().get_reference_by_name(&type_param_name).unwrap();
            let type_heap_ref = type_ref.to_heap_ref().unwrap();
            heap.load_type_reference(type_heap_ref.address).unwrap()
        }
    };

    let params: Vec<KindHash> = kind_hash_tree
        .params
        .into_iter()
        .map(|p| resolve_tree(p, environment, heap))
        .collect();

    if !params.is_empty() {
        let joined = params.join(",");
        KindHash::from(format!("{}<<{}>>", base, joined))
    } else {
        base
    }
}

pub(crate) fn resolve_kind_hash(kind_hash: &KindHash, environment: &Arc<RwLock<Environment>>, heap: &Heap) -> KindHash {
    let tokens = KindHashTokenizer::of(kind_hash).tokenize();
    let tree = KindHashParser::of(tokens).parse();
    resolve_tree(tree, environment, heap)
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

// mod::name
// {T}
enum Type {
    KindHash(KindHash),
    TypeParam(String),
}

// mod::name
// {T}
// mod::name<<other_mod::nested>>
// mod::name<<other_mod::nested, {T}>>
struct KindHashTree {
    ty: Type,
    params: Vec<KindHashTree>
}
