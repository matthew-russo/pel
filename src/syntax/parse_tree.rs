use std::collections::HashMap;

#[derive(Debug, Clone)]
pub(crate) struct Program {
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone)]
pub(crate) enum Declaration {
    EnumDeclarationNode(EnumDeclaration),
    ContractDeclarationNode(ContractDeclaration),
    ImplementationDeclarationNode(ImplementationDeclaration),
    ObjectDeclarationNode(ObjectDeclaration),
    FunctionDeclarationNode(FunctionDeclaration),
}

#[derive(Debug, Clone)]
pub(crate) struct EnumDeclaration {
    pub type_name: String,
    pub type_params: Vec<String>,
    pub variants: Vec<VariantDeclaration>,
    pub methods: Vec<FunctionDeclaration>,
}

#[derive(Debug, Clone)]
pub(crate) struct ObjectDeclaration {
    pub type_name: String,
    pub type_params: Vec<String>,
    pub fields: Vec<TypedVariableDeclaration>,
    pub methods: Vec<FunctionDeclaration>,
}

#[derive(Debug, Clone)]
pub(crate) struct ContractDeclaration {
    pub type_name: String,
    pub type_params: Vec<String>,
    pub functions: Vec<FunctionSignature>,
}

#[derive(Debug, Clone)]
pub(crate) struct ImplementationDeclaration {
    pub implementing_type: Expression,
    pub contract: Expression,
    pub functions: Vec<FunctionDeclaration>,
}

#[derive(Debug, Clone)]
pub(crate) struct VariantDeclaration {
    pub name: String,
    pub contains: Option<Expression>,
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionDeclaration {
    pub visibility: Option<Visibility>,
    pub signature: FunctionSignature,
    pub body: BlockBody,
}

#[derive(Debug, Clone)]
pub(crate) enum Visibility {
    Public,
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionSignature {
    pub name: String,
    pub type_parameters: Vec<String>,
    pub parameters: Vec<FunctionParameter>,
    pub returns: Option<Expression>
}

#[derive(Debug, Clone)]
pub(crate) enum FunctionParameter {
    SelfParam,
    TypedVariableDeclarationParam(TypedVariableDeclaration)
}

#[derive(Debug, Clone)]
pub(crate) struct GenericIdentifier {
    pub id: String,
    pub wrapped_type: Expression,
}

#[derive(Debug, Clone)]
pub(crate) struct TypedVariableDeclaration {
    pub name: String,
    pub type_reference: Expression,
}

#[derive(Debug, Clone)]
pub(crate) struct BlockBody {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub(crate) enum Statement {
    VariableAssignmentNode(VariableAssignment),
    ReturnNode(Return),
    ExpressionNode(Expression),
}

// TODO -> An expression target can be wrong
#[derive(Debug, Clone)]
pub(crate) struct VariableAssignment {
    pub target: Expression,
    pub target_type: Expression,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub(crate) struct Return {
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub(crate) enum Expression {
    ChainableExpressionNode(Box<ChainableExpression>),
    UnaryOperationNode(Box<UnaryOperation>),
    LambdaNode(Box<Lambda>),
}

#[derive(Debug, Clone)]
pub(crate) struct Lambda {
    pub params: Vec<TypedVariableDeclaration>,
    pub body: BlockBody,
}

#[derive(Debug, Clone)]
pub(crate) struct ChainableExpression {
    pub start: ExpressionStart,
    pub chained: Vec<ExpressionChain>,
}

#[derive(Debug, Clone)]
pub(crate) enum ExpressionStart {
    ConditionalNode(Conditional),
    MatchNode(Match),
    LoopNode(Loop),
    VariableNode(Variable),
    ValueNode(Value),
}

#[derive(Debug, Clone)]
pub(crate) enum ExpressionChain {
    FieldAccessNode(FieldAccess),
    ModuleAccessNode(ModuleAccess),
    ObjectInitializationNode(ObjectInitialization),
    FunctionApplicationNode(FunctionApplication),
    TypeApplicationNode(TypeApplication),
    BinaryOperationNode(BinaryOperation),
}

#[derive(Debug, Clone)]
pub(crate) struct UnaryOperation {
    pub op: UnaryOperator,
    pub expr: ChainableExpression,
}

#[derive(Debug, Clone)]
pub(crate) struct BinaryOperation {
    pub op: BinaryOperator,
    pub rhs: Expression,
}

#[derive(Debug, Clone)]
pub(crate) struct FieldAccess {
    pub field_name: String
}

#[derive(Debug, Clone)]
pub(crate) struct ModuleAccess {
    pub name: String
}

#[derive(Debug, Clone)]
pub(crate) struct TypeApplication {
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionApplication {
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub(crate) enum Variable {
    Name(String),
    SelfVariable,
    SelfType
}

#[derive(Debug, Clone)]
pub(crate) enum Value {
    BooleanValue(bool),
    StringValue(String),
    CharValue(char),
    IntegerValue(i32),
    FloatValue(f32),
}

impl Value {
    fn to_int(&self) -> Option<i32> {
        match self {
            Value::IntegerValue(i) => Some(*i),
            _ => None
        }
    }

    fn to_bool(&self) -> Option<bool> {
        match self {
            Value::BooleanValue(b) => Some(*b),
            _ => None
        }
    }

    // TODO -> This should take floats too
    pub fn add(lhs: Self, rhs: Self) -> Self {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Value::IntegerValue(lhs + rhs);
    }

    // TODO -> This should take floats too
    pub fn subtract(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Value::IntegerValue(lhs - rhs);
    }

    // TODO -> This should take floats too
    pub fn multiply(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Value::IntegerValue(lhs * rhs);
    }

    // TODO -> This should take floats too
    pub fn divide(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Value::IntegerValue(lhs / rhs);
    }

    // TODO -> This should take floats too
    pub fn less_than(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Value::BooleanValue(lhs < rhs);
    }

    // TODO -> This should take floats too
    pub fn less_than_or_equal(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Value::BooleanValue(lhs <= rhs);
    }

    // TODO -> This should take floats too
    pub fn greater_than(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Value::BooleanValue(lhs > rhs);
    }

    // TODO -> This should take floats too
    pub fn greater_than_or_equal(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Value::BooleanValue(lhs >= rhs);
    }

    // TODO -> This should take any value type
    pub fn equal_to(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Value::BooleanValue(lhs == rhs);
    }

    // TODO -> This should take any value type
    pub fn not_equal_to(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_int().unwrap();
        let rhs = rhs.to_int().unwrap();
        return Value::BooleanValue(lhs != rhs);
    }

    pub fn or(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_bool().unwrap();
        let rhs = rhs.to_bool().unwrap();
        return Value::BooleanValue(lhs || rhs);
    }

    pub fn and(lhs: Self, rhs: Self) -> Value {
        let lhs = lhs.to_bool().unwrap();
        let rhs = rhs.to_bool().unwrap();
        return Value::BooleanValue(lhs && rhs);
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Conditional {
    pub if_exprs: Vec<If>,
    pub else_expr: Option<BlockBody>,
}

#[derive(Debug, Clone)]
pub(crate) struct If {
    pub condition: Expression,
    pub body: BlockBody,
}

#[derive(Debug, Clone)]
pub(crate) struct Match {
    pub expr: Expression,
    pub patterns: Vec<Pattern>,
}

#[derive(Debug, Clone)]
pub(crate) struct Pattern {
    pub to_match: Expression,
    pub body: BlockBody,
}

#[derive(Debug, Clone)]
pub(crate) struct Loop {
    // TODO -> this doesn't need to be a variable assignment
    pub init: VariableAssignment,
    pub condition: Expression,
    pub step: Statement,
    pub body: BlockBody,
}

#[derive(Debug, Clone)]
pub(crate) struct ObjectInitialization {
    pub fields: HashMap<String, Expression>,
}

#[derive(Debug, Clone)]
pub(crate) enum UnaryOperator {
    Not,
    // Increment,
    // Decrement,
    // BitwiseNot,
}

#[derive(Debug, Clone)]
pub(crate) enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    Equal,
    NotEqual,
    Or,
    And,
//     BitwiseOr,
//     BitwiseAnd,
//     BitwiseXor,
//     BitwiseShiftLeft,
//     BitwiseShiftRight,
}

