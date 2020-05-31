use std::collections::HashMap;
use crate::utils::LocationContext;

#[derive(Debug, Clone)]
pub(crate) struct Program {
    pub location: LocationContext,
    pub declarations: Vec<Declaration>,
}

#[derive(Debug, Clone)]
pub(crate) enum TypeIdentifier {
    GenericTypeNode(GenericType),
    ArrayTypeNode(Box<ArrayType>),
}

impl TypeIdentifier {
    pub(crate) fn location(&self) -> LocationContext {
        match self {
            TypeIdentifier::GenericTypeNode(gt) => gt.location.clone(),
            TypeIdentifier::ArrayTypeNode(at) => at.location.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct GenericType {
    pub location: LocationContext,
    pub name: String,
    pub type_application: Option<TypeApplication>,
}

#[derive(Debug, Clone)]
pub(crate) struct GenericName {
    pub location: LocationContext,
    pub name: String,
    pub type_parameters: Vec<String>,
}

#[derive(Debug, Clone)]
pub(crate) enum Declaration {
    ModuleDeclarationNode(ModuleDeclaration),
    EnumDeclarationNode(EnumDeclaration),
    ContractDeclarationNode(ContractDeclaration),
    ImplementationDeclarationNode(ImplementationDeclaration),
    ObjectDeclarationNode(ObjectDeclaration),
    FunctionDeclarationNode(FunctionDeclaration),
    UseDeclarationNode(UseDeclaration),
}

impl Declaration {
    pub(crate) fn location(&self) -> LocationContext {
        match self {
            Declaration::ModuleDeclarationNode(md) => md.location.clone(),
            Declaration::EnumDeclarationNode(ed) => ed.location.clone(),
            Declaration::ContractDeclarationNode(cd) => cd.location.clone(),
            Declaration::ImplementationDeclarationNode(id) => id.location.clone(),
            Declaration::ObjectDeclarationNode(od) => od.location.clone(),
            Declaration::FunctionDeclarationNode(fd) => fd.location.clone(),
            Declaration::UseDeclarationNode(ud) => ud.location.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct ModuleDeclaration {
    pub location: LocationContext,
    pub mod_name: String,
    pub functions: Vec<FunctionDeclaration>,
}

#[derive(Debug, Clone)]
pub(crate) struct EnumDeclaration {
    pub location: LocationContext,
    pub name: GenericName,
    pub variants: Vec<VariantDeclaration>,
    pub methods: Vec<FunctionDeclaration>,
}

#[derive(Debug, Clone)]
pub(crate) struct ObjectDeclaration {
    pub location: LocationContext,
    pub name: GenericName,
    pub fields: Vec<TypedVariableDeclaration>,
    pub methods: Vec<FunctionDeclaration>,
}

#[derive(Debug, Clone)]
pub(crate) struct ContractDeclaration {
    pub location: LocationContext,
    pub name: GenericName,
    pub functions: Vec<FunctionSignature>,
}

#[derive(Debug, Clone)]
pub(crate) struct ImplementationDeclaration {
    pub location: LocationContext,
    pub implementing_type: GenericName, // TODO
    pub contract: GenericName, // TODO
    pub functions: Vec<FunctionDeclaration>,
}

#[derive(Debug, Clone)]
pub(crate) struct VariantDeclaration {
    pub location: LocationContext,
    pub name: String,
    pub contains: Option<TypeIdentifier>,
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionDeclaration {
    pub location: LocationContext,
    pub visibility: Option<Visibility>,
    pub signature: FunctionSignature,
    pub body: BlockBody,
}

#[derive(Debug, Clone)]
pub(crate) struct UseDeclaration {
    pub location: LocationContext,
    pub import_chain: Vec<String>,
}

#[derive(Debug, Clone)]
pub(crate) enum Visibility {
    Public,
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionSignature {
    pub location: LocationContext,
    pub name: GenericName,
    pub parameters: Vec<FunctionParameter>,
    pub returns: Option<TypeIdentifier>
}

#[derive(Debug, Clone)]
pub(crate) enum FunctionParameter {
    SelfParam,
    TypedVariableDeclarationParam(TypedVariableDeclaration)
}

#[derive(Debug, Clone)]
pub(crate) struct TypedVariableDeclaration {
    pub location: LocationContext,
    pub name: String,
    pub type_identifier: TypeIdentifier,
}

#[derive(Debug, Clone)]
pub(crate) struct BlockBody {
    pub location: LocationContext,
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub(crate) enum Statement {
    VariableAssignmentNode(VariableAssignment),
    ReturnNode(Return),
    ExpressionNode(Expression),
}

impl Statement {
    pub(crate) fn location(&self) -> LocationContext {
        match self {
            Statement::VariableAssignmentNode(va) => va.location.clone(),
            Statement::ReturnNode(r) => r.location.clone(),
            Statement::ExpressionNode(e) => e.location(),
        }
    }
}

// TODO -> An expression target can be wrong
#[derive(Debug, Clone)]
pub(crate) struct VariableAssignment {
    pub location: LocationContext,
    pub target: Expression,
    pub target_type: TypeIdentifier,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub(crate) struct Return {
    pub location: LocationContext,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub(crate) enum Expression {
    ChainableExpressionNode(Box<ChainableExpression>),
    UnaryOperationNode(Box<UnaryOperation>),
    LambdaNode(Box<Lambda>),
}

impl Expression {
    pub(crate) fn location(&self) -> LocationContext {
        use Expression::*;

        match self {
            ChainableExpressionNode(ce_box) => ce_box.location.clone(),
            UnaryOperationNode(uo_box) => uo_box.location.clone(),
            LambdaNode(lambda_box) => lambda_box.location.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Lambda {
    pub location: LocationContext,
    pub params: Vec<TypedVariableDeclaration>,
    pub body: BlockBody,
}

#[derive(Debug, Clone)]
pub(crate) struct ChainableExpression {
    pub location: LocationContext,
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
    ArrayType(ArrayType),
    ArrayInitialization(ArrayInitialization),
}

impl ExpressionStart {
    pub(crate) fn location(&self) -> LocationContext {
        match self {
            ExpressionStart::ConditionalNode(cn) => cn.location.clone(),
            ExpressionStart::MatchNode(mn) => mn.location.clone(),
            ExpressionStart::LoopNode(ln) => ln.location.clone(),
            ExpressionStart::VariableNode(vn) => match vn {
                Variable::Name((_, lc))    => lc.clone(),
                Variable::SelfVariable(lc) => lc.clone(),
                Variable::SelfType(lc)     => lc.clone(),
            },
            ExpressionStart::ValueNode(vn) => match vn {
                Value::BooleanValue((_, lc)) => lc.clone(),
                Value::StringValue((_, lc))  => lc.clone(),
                Value::CharValue((_, lc))    => lc.clone(),
                Value::IntegerValue((_, lc)) => lc.clone(),
                Value::FloatValue((_, lc))   => lc.clone(),
            },
            ExpressionStart::ArrayType(at) => at.location.clone(),
            ExpressionStart::ArrayInitialization(ai) => ai.location.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum ExpressionChain {
    FieldAccessNode(FieldAccess),
    ModuleAccessNode(ModuleAccess),
    ArrayAccessNode(ArrayAccess),
    ObjectInitializationNode(ObjectInitialization),
    FunctionApplicationNode(FunctionApplication),
    TypeApplicationNode(TypeApplication),
    BinaryOperationNode(BinaryOperation),
}

impl ExpressionChain {
    pub(crate) fn location(&self) -> LocationContext {
        match self {
            ExpressionChain::FieldAccessNode(fan) => fan.location.clone(),
            ExpressionChain::ModuleAccessNode(man) => man.location.clone(),
            ExpressionChain::ArrayAccessNode(aan) => aan.location.clone(),
            ExpressionChain::ObjectInitializationNode(oin) => oin.location.clone(),
            ExpressionChain::FunctionApplicationNode(fan) => fan.location.clone(),
            ExpressionChain::TypeApplicationNode(tan) => tan.location.clone(),
            ExpressionChain::BinaryOperationNode(bon) => bon.location.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct UnaryOperation {
    pub location: LocationContext,
    pub op: UnaryOperator,
    pub expr: ChainableExpression,
}

#[derive(Debug, Clone)]
pub(crate) struct BinaryOperation {
    pub location: LocationContext,
    pub op: BinaryOperator,
    pub rhs: Expression,
}

#[derive(Debug, Clone)]
pub(crate) struct FieldAccess {
    pub location: LocationContext,
    pub field_name: String
}

#[derive(Debug, Clone)]
pub(crate) struct ModuleAccess {
    pub location: LocationContext,
    pub name: String
}

#[derive(Debug, Clone)]
pub(crate) struct ArrayAccess {
    pub location: LocationContext,
    pub index_expr: Expression,
}

#[derive(Debug, Clone)]
pub(crate) struct TypeApplication {
    pub location: LocationContext,
    pub args: Vec<TypeIdentifier>,
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionApplication {
    pub location: LocationContext,
    pub args: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub(crate) enum Variable {
    Name((String, LocationContext)),
    SelfVariable(LocationContext),
    SelfType(LocationContext)
}

#[derive(Debug, Clone)]
pub(crate) enum Value {
    BooleanValue((bool, LocationContext)),
    StringValue((String, LocationContext)),
    CharValue((char, LocationContext)),
    IntegerValue((i32, LocationContext)),
    FloatValue((f32, LocationContext)),
}

#[derive(Debug, Clone)]
pub(crate) struct ArrayType {
    pub location: LocationContext,
    pub ty: TypeIdentifier,
}

#[derive(Debug, Clone)]
pub(crate) struct ArrayInitialization {
    pub location: LocationContext,
    pub ty: Expression,
    pub size: Expression,
}

#[derive(Debug, Clone)]
pub(crate) struct Conditional {
    pub location: LocationContext,
    pub if_exprs: Vec<If>,
    pub else_expr: Option<BlockBody>,
}

#[derive(Debug, Clone)]
pub(crate) struct If {
    pub location: LocationContext,
    pub condition: Expression,
    pub body: BlockBody,
}

#[derive(Debug, Clone)]
pub(crate) struct Match {
    pub location: LocationContext,
    pub expr: Expression,
    pub patterns: Vec<Pattern>,
}

#[derive(Debug, Clone)]
pub(crate) struct Pattern {
    pub location: LocationContext,
    pub to_match: Expression,
    pub body: BlockBody,
}

#[derive(Debug, Clone)]
pub(crate) struct Loop {
    pub location: LocationContext,
    // TODO -> this doesn't need to be a variable assignment
    pub init: VariableAssignment,
    pub condition: Expression,
    pub step: Statement,
    pub body: BlockBody,
}

#[derive(Debug, Clone)]
pub(crate) struct ObjectInitialization {
    pub location: LocationContext,
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

