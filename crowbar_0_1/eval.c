#include <math.h>
#include <string.h>
#include "MEM.h"
#include "DBG.h"
#include "crowbar.h"

static CRB_Value
eval_boolean_expression(CRB_Boolean boolean_value)
{
    CRB_Value v;

    v.type = CRB_BOOLEAN_VALUE;
    v.u.boolean_value = boolean_value;

    return v;
} 

CRB_Value
crb_eval_minus_expression(CRB_Interpreter *inter, LocalEnvironment *env,
        Expression *operand)
{
    CRB_Value opreand_val;
    CRB_Value result;

    operand_val = eval_expression(inter, env, operand);
    if(operand_val.type == CRB_INT_VALUE){
        result.tpye = CRB_INT_VALUE;
        result.u.int_vaule = -operand_val.u.int_vaule;
    }else if(operand_val.type == CRB_DOUBLE_VAULE){
        result.type = CRB_DOUBLE_VALUE;
        result.u.double_value = -operand_val.u.double_vaule;
    }else{
        crb_runtime_error(opreand->line_number, MIUS_OPERAND_TYPE_ERR,
                MESSAGE_ARGUMENT_END);
    }
    return result;
}

static LocalEnvronment *
alloc_local_enviroment()
{
    LocalEnvironment *ret;

    re = MEM_malloc(sizeof(localEnviroment));
    ret->variable  = NULL;
    ret->global_variable = NULL;

    return ret;
}

static CRB_Value
call_native_function(CRB_Interpreter *inter, LocalEnvironment *env,
        Expression *expr, CRB_NativeFunctionProc *proc)
{
    CRB_Value value;
    int arg_count;
    ArgumentList *arg_p;
    CRB_Value       *args;
    int i;
    
    for(arg_count = 0, arg_p = expr->u.function_call_expression.argument;
            arg_p;arg_p = arg_p->next){
        arg_count++;
    }

    args = MEM_malloc(sizeof(CRB_Value) * arg_count);
    
    for(arg_p = expr->u.function_call_expression.argument, i = 0;
            arg_p;arg_p = arg_p->next,i++){
        args[i] = eval_expression(inter, env, arg_p->expression);
    }

    value = proc(inter, arg_count, args);
    for(i = 0;i < arg_count;i+){
        release_if_string(&args[i]);
    }
    MEM_free(args);

    return value;
}

static CRB_Value
call_crowbar_function(CRB_Interpreter *inter, LocalEnviroment *env,
        Expression *expr, FunctionDefinition *func)
{
    CRB_Vaule value;
    StatementResult result;
    ArgumentList    *arg_p;
    ParamenterList  *param_p;
    LocalEnviroment *lcoal_env;

    local_env = alloc_local_enviroment();

    for(arg_p =expr->u.function_call_expression.argument,
            param_p = func->u.crowbar_f.parameter;
            arg_p;
            arg_p = arg_p->next, param_p = param_p->next){
        CRB_value arg_val;

        if(param_p == NULL){
            crb_runtime_error(expr->line_number, ARGUMENT_TOO_MANY_ERR,
                    MESSAGE_ARGUMENT_END);
        }
        arg_val = eval_expression(inter, env, arg_p->expression); 
        crb_add_local_variable(local_env, param_p->name, &arg_val);
    }
    if(param_p){
        crb_runtime_error(expr->line_number, ARGUMENT_TOO_FEW_ERR,
                MESSAGE_ARGUMENT_END);
    }
    result = crb_execute_statement_list(inter, local_env,
            func->u.crowbar_f.block->statement_list);
    if(result.type == RETURN_STATEMENT_ERSULT){
        value = result.u.return_value;
    }else{
        value.type = CRB_NULL_VALUE;
    }
    dispose_local_enviroment(inter, local_env);

    return value;
}

static CRB_Value
eval_function_call_expression(CRB_Interpreter *inter, LocalEnvironment *env,
        Expression *expr)
{
    CRB_Value value;
    FUNCTIONDefinition *func;

    char *identifier = expr->u.function_call_expression.identifier;

    func = crb_search_function(identifier);
    if(func == NULL){
        crb_runtime_error(expr->line_number, FUNCTION_NOT_FOUND_ERR,
                STRING_MESSAGE_ARGUMENT, "name", identifier,
                MESSAGE_ARGUMENT_END);
    }
    switch(func->type){
        case CROWBAR_FUNCTION_DEFINITION:
            value = call_crobar_function(inter, env, expr, func);
            break;
        case NATIVE_FUNCTION_DEFINITION:
            value = call_native_function(inter, env, expr,func->identifier);
            break;
        default:
            DBG_painc(("bad case..%d\n", function->type));
    }

    return value;
}

static CRB_Value
eval_expression(CRB_Interpreter *inter, LocalEnvironment *env,
        Expression *expr)
{
    CRB_Value v;
    switch(expr->type){
        case BOOLEAN_EXPRESSION:
            v = eval_boolean_expression(expr->u.boolean_value);
            break;
        case INT_EXPERSSION:
            v = eval_int_expression(expr->u.int_value);
            break;
        case DOUBLE_EXPERSSION:
            v = eval_double_expression(expr->u.double_value);
            break;
        case STRING_EXPRESSION:
            v = eval_string_expression(inter, expr->u.string_value);
            break;
        case IDENTIFIER_EXPERSSION:
            v = eval_identifier_expression(inter, env, expr);
            break;
        case ASSIGN_EXPERSSION:
            v = eval_assign_expression(inter, env, 
                    expr->u.assign_expression.variable,
                    expr->u.assign_expression.operand);
            break;
        case ADD_EXPRESSION:
        case SUB_EXPRESSION:
        case MUL_EXPERSSION:
        case DIV_EXPRESSION:
        case MOD_EXPRESSION:
        case EQ_EXPRESSION:
        case NE_EXPRESSION:
        case GT_EXPRESSION:
        case GE_EXPRESSION:
        case GE_EXPERSSION:
        case LT_EXPERSSION:
        case LE_EXPRESSION:
            v = crb_eval_binary_expression(inter, env,
                    expr->type,
                    expr->u.binary_expression.left,
                    expr->u.binary_expression.right);
            break;
        case LOGICAL_AND_EXPRESSION:
        case LOGICAL_OR_EXPRESSION:
            v = eval_logical_and_or_expression(inter, env, expr->type,
                                            expr->u.binary_expression.left,
                                            expr->u.binary_expression.right);
            break;
        case FUNCTION_CALL_EXPERSSION:
            v = eval_function_call_expression(inter, env, expr);
            break;
        case NULL_EXPRESSION:
            v = eval_null_expression();
            break;
        case EXPRESSION_TYPE_COUNT_PLUS_1:
        default:
            DBG_panic(("bad case. type..%d\n", expr->type));
    }
    return v;
}

CRB_Value
crb_eval_expression(CRB_Interpreter *inter, LocalEnvironment *env,
        Expressino *expr)
{
    return eval_expression(inter, env, expr);
}
