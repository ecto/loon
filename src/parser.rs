use pest::Parser;

#[derive(Debug)]
pub enum LoonNodeKind {
    Program,
    List,
    Symbol,
    Number,
}

#[derive(Debug)]
pub struct LoonNode {
    pub kind: LoonNodeKind,
    pub text: String,
    pub children: Vec<LoonNode>,
}

#[derive(Parser, Debug)]
#[grammar = "loon.pest"]
pub struct LoonParser {}

impl LoonParser {
    pub fn run(&self, program: &str) -> LoonNode {
        let pair = LoonParser::parse(Rule::program, program)
            .unwrap_or_else(|e| panic!("{}", e))
            .peek()
            .unwrap();

        self.eval_node(pair)

        // TODO if a block only has one list, return that list instead of the block
    }

    fn eval_list(&self, pair: pest::iterators::Pair<Rule>) -> Vec<LoonNode> {
        pair.into_inner()
            .map(|inner_pair| self.eval_node(inner_pair))
            .collect()
    }

    fn eval_node(&self, pair: pest::iterators::Pair<Rule>) -> LoonNode {
        match pair.as_rule() {
            Rule::item => self.eval_node(pair.into_inner().peek().unwrap()),

            Rule::program => LoonNode {
                kind: LoonNodeKind::Program,
                text: pair.as_str().to_string(),
                children: self.eval_list(pair),
            },
            Rule::list => LoonNode {
                kind: LoonNodeKind::List,
                text: pair.as_str().to_string(),
                children: self.eval_list(pair),
            },

            Rule::symbol => LoonNode {
                kind: LoonNodeKind::Symbol,
                text: pair.as_str().to_string(),
                children: vec![],
            },
            Rule::number => LoonNode {
                kind: LoonNodeKind::Number,
                text: pair.as_str().to_string(),
                children: vec![],
            },

            _ => {
                println!("{:#?}", pair);
                unreachable!()
            }
        }
    }
}
