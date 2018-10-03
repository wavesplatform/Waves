# Types
|type|adds|
|-------|---|
{{#types}}| <a name="{{name}}">{{name}}</a> | {{#isNative}}Native | {{/isNative}}  {{#isUnion}}{{#union}} [{{name}}](#{{name}}){{/union}}{{/isUnion}}{{#isObj}}<ul>{{#fields}} <li> **{{name}}**{{#type}}{{#isNative}}  [{{name}}](#{{name}}){{/isNative}}{{#isObj}} [{{name}}](#{{name}}){{/isObj}}{{#isUnion}}{{#union}} [{{name}}](#{{name}}){{/union}}{{/isUnion}} {{#hasParam}} [{{name}}](#{{name}})[{{#param}} [{{name}}](#{{name}}){{/param}}]{{/hasParam}}{{/type}}</li>{{/fields}}</ul>| {{/isObj}}
{{/types}}

# Input variables
|vars|type|doc|
|-------|---|---|
{{#vars}}| {{name}}|{{#type}}{{#isNative}} [{{name}}](#{{name}}) |{{/isNative}}{{#isObj}}  [{{name}}](#{{name}})| {{/isObj}}{{#isUnion}}{{#union}} [{{name}}](#{{name}}){{/union}}| {{/isUnion}}{{#hasParam}} {{name}}[{{#param}}[{{name}}](#{{name}}){{/param}}]{{/hasParam}}{{/type}} {{doc}}|
{{/vars}}


# Functions
|funcs|doc|params|type|
|-------|---|---|---|
{{#funcs}}| {{name}}|{{doc}}|<ul>{{#params}} <li>{{name}}{{#type}}{{#isComplex}} {{name}}{{/isComplex}}{{#isNative}} [{{name}}](#{{name}}){{/isNative}}{{#isObj}} [{{name}}](#{{name}}){{/isObj}}{{#isUnion}}{{#union}} [{{name}}](#{{name}}){{/union}}{{/isUnion}}{{#hasParam}} {{name}}[{{#param}}{{#needLink}} [{{name}}](#{{name}}){{/needLink}}{{#noLink}} {{name}}{{/noLink}}{{/param}}]{{/hasParam}}{{/type}} {{doc}}</li>{{/params}}</ul>|{{#type}}{{#isComplex}}  {{name}}{{/isComplex}}{{#isNative}} [{{name}}](#{{name}}){{/isNative}}{{#isObj}} [{{name}}](#{{name}}){{/isObj}}{{#isUnion}}{{#union}} [{{name}}](#{{name}}){{/union}}{{/isUnion}}{{#hasParam}} {{name}}[{{#param}}{{#needLink}} [{{name}}](#{{name}}){{/needLink}}{{#noLink}} {{name}}{{/noLink}}{{/param}}] {{/hasParam}}{{/type}}
{{/funcs}}