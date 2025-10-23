def _makecodes(*names):
    items = [(i, name) for i, name in enumerate(names)]
    globals().update({name: i for i, name in items})
    return items

result = _makecodes('FAILURE', 'SUCCESS', 'ANY')
print(result)
print(FAILURE)
print(SUCCESS)
print(ANY)
