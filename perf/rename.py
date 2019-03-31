import collections
import string
import sys


CONST_CHARS = set(string.ascii_uppercase + string.digits + '_')
ESCAPE_CHAR = '\\'
ID_FIRST_CHARS_STR = string.ascii_letters
ID_CHARS_STR = ID_FIRST_CHARS_STR + string.digits + '_$'
ID_CHARS = set(ID_CHARS_STR)
KEYWORDS = {
    'var', 'function', 'return', 'new', 'throw', 'instanceof', 'try', 'catch',
    'if', 'typeof', 'for', 'else', 'null', 'in', 'Object', 'prototype', 'Array',
    'switch', 'case', 'String', 'Math', 'append', 'constructor', 'apply',
    'bind', 'create', 'map', 'filter', 'exports', 'Error', 'length', 'this',
    'toString', 'eval', 'Infinity', 'void', 'replace', 'break', 'floor', 'push',
    'slice', 'hasOwnProperty', 'keys', 'defineProperty', 'fromCharCode',
    'charCodeAt', 'getObjectById', 'console', 'log', 'default', 'from',
    'call', 'parseInt', 'message', 'RegExp', 'splice', 'next', 'add',
    'test', 'concat', 'discard', 'top', 'bottom', 'random', 'delete',
    'debugger', 'do', 'finally', 'while', 'with',
}
QUOTE_CHARS = '"' + "'"
REGEX_CHAR = '/'
REGEX_PREV_CHAR = '('
SAFE_IDS = {
    'type_', 'active', 'simplify', 'pure', 'inc', 'lift', 'state', 'def', 'eq',
    'partialEmpty', 'partialAppend', 'show', 'mempty', 'badReturnCode',
    'renderError', 'tailRecM', 'liftEffect', 'index', 'localCreate', 'ask',
    'tell', 'listen', 'tailRecM', 'isUndefined', 'traverse', 'head', 'tail',
    'sequence', 'hasProperty', 'wrap', 'unwrap', 'throwError', 'catchError',
    'run', 'errorMessage', 'executeAction', 'fromEnum', 'toEnum', 'main',
    'executeAction', 'mapWithIndex', 'validate', 'one', 'zero', 'empty',
    'compare', 'compose', 'identity', 'bimap', 'conj', 'disj', 'implies',
    'not', 'mul', 'sub', 'alt', 'recip', 'defer', 'pass', 'agents', 'pred',
    'succ', 'cardinality', 'errorAt', 'moved', 'other',
}
SAFE_IDS = {}
SAFE_SUBWORDS = {'fold', 'codeJson'}
UNSAFE_IDS = {
    'find', 'energyCapacity', 'energy', 'carry', 'constructor', 'name', 'value',
    'room', 'pos', 'Memory', 'Game', 'gcl', 'rcl', 'controller', 'reserve',
    'owner', 'structureType', 'carryCapacity', 'spawning', 'username',
}


def infer_token_type(type, token):
    if type == 'id':
        if all(char in string.digits for char in token):
            return 'number'
        if token in KEYWORDS:
            return 'keyword'
        if token in SAFE_IDS:
            return 'id'
        if token in UNSAFE_IDS:
            return 'unsafe'
        if all(char in CONST_CHARS for char in token):
            return 'unsafe'
        if len(token) <= 2:
            return 'short'
        if '$' in token:
            return 'id'
        if token[-1] in string.digits:
            return 'id'
        if any(subword in token for subword in SAFE_SUBWORDS):
            return 'id'
        if '_uncurried' in token:
            return 'id'
        if token[0] == '_':
            return 'id'
        return 'unknown'
    return type


def tokenize(code):
    token = []
    quote = None
    escape = False
    type = None

    def next_token():
        if token:
            assert type is not None
            token_str = ''.join(token)
            yield (infer_token_type(type, token_str), token_str)
            token[:] = []
            locals()['type'] = None

    def next_char(char):
        assert type is not None
        token.append(char)

    def is_quote(char):
        if escape:
            return False
        if char in QUOTE_CHARS:
            return True
        if char == REGEX_CHAR:
            if quote is None and token and token[-1] in REGEX_PREV_CHAR:
                return True
            if quote == char:
                return True
        return False

    for char in code:
        if char == ESCAPE_CHAR:
            if escape:
                escape = False
        elif is_quote(char):
            if quote is None:
                yield from next_token()
                quote = char
                type = 'string'
                next_char(char)
            elif quote == char:
                quote = None
                next_char(char)
                yield from next_token()
            continue
        if quote is None:
            id_char = char in ID_CHARS
            if id_char != (type == 'id'):
                yield from next_token()
                type = 'id' if id_char else 'punct'
        next_char(char)
    yield from next_token()


def detokenize(tokens):
    return ''.join(token for (_, token) in tokens)


def compute_reserved_ids(tokens):
    return {
        token
        for (type, token) in tokens
        if type in ('short', 'keyword', 'unsafe', 'unknown')
    } | KEYWORDS


def generate_ids(first_char=True):
    if first_char:
        chars = ID_FIRST_CHARS_STR
    else:
        chars = ID_CHARS_STR
    for char in chars:
        yield char
    for rest in generate_ids(first_char=False):
        for char in chars:
            yield char + rest


def assign_ids(tokens, id_generator):
    ids = [token for (type, token) in tokens if type == 'id']
    histogram = collections.Counter(ids)
    return {id: next(id_generator) for (id, _) in histogram.most_common()}


def rename_ids(tokens, id_mapping):
    def rename_id(type, token):
        if type == 'id':
            return id_mapping[token]
        return token
    return [(type, rename_id(type, token)) for (type, token) in tokens]


def main(path):
    with open(path, 'r') as file:
        code = ''.join(file)
        tokens = list(tokenize(code))
        reserved_ids = compute_reserved_ids(tokens)
        id_generator = (id for id in generate_ids() if id not in reserved_ids)
        id_mapping = assign_ids(tokens, id_generator)
        tokens = rename_ids(tokens, id_mapping)
        print(detokenize(tokens))

if __name__ == '__main__':
    main(sys.argv[1])
