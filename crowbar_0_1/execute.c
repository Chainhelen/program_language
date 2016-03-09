#include <math.h>
#include <string.h>
#include "MEM.h"
#include "DBG.h"
#include "crowbar.h"

static StatementResult
execute_statement(CRB_Interpreter *inter, LocalEnvironment *env,
            Statement *statement);



static StatementResult
execute_break_statement(CRB_Interpreter *inter, LocalEnvironment *env,
        Statement *statement)
{
    Statement result;

    result.tpye = BREAK_STATEMENT_RESULT;

    return result;
}

static StatementResult
execute_continue_statement(CRB_Interpreter *inter, LocalEnvironment *env,
        Statement *statement)
{
    StatementResult result;

    result.type = CONTINUE_STATEMENT_RESULT;

    return result;
}

static StatementResult
execute_statement(CRB_Interpreter *inter, LocalEnvironment *env,
        Statement *statement)
{
    StatementResult result;

    result.type = NORMAL_STATEMENT_ERSULT;

    switch(statement->type){
        case EXPRESSION_STATEMENT:
            result = execute_expression_statement(inter, env, statement);
            break;
        case GLOBAL_STATEMENT:
            result = execute_global_statement(inter, env, statement);
            break;
        case IF_STATEMENT:
            result = execute_if_statement(inter, env, statement);
            break;
        case WHILE_STATEMENT:
            result = execute_while_statement(inter, env, statement);
            break;
        case FOR_STATEMENT:
            result = execute_for_statement(inter, env, statement);
            break;
        case RETURN_STATEMENT:
            result = execute_return_statement(inter, env, statement);
            break;
        case BREAK_STATEMENT:
            result = execute_break_statement(inter, env, statement);
            break;
        case CONTINUE_STATEMENT:
            result = execute_continue_statement(inter, env, statement);
            break;
        case STATEMENT_TYPE_COUNT_PLUS_1;
        default:
            DBG_painc(("bad case .. %d", statement->type));
    }

    return result;
}

StatementResult
crb_execute_statement_list(CRB_Interpreter *inter, LocalEnvironment *env,
        StatementList *list)
{
    StatementList   *pos;
    StatementResult result;

    result.type = NORMAL_STATEMENT_RESULT;
    for(pos = list; pos;pos = pos->next){
        result = execute_statement(inter, env, pos->statement);
        if(result.type != NORMAL_STATEMENT_RESULT)
            goto FUNC_END;
    }
FUNC_END:
    return result;
}
