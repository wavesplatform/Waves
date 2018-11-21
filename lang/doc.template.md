# Types
|type|adds|
|-------|---|
{{#types}}| [{{name}}](#{{name}}) | {{#isNative}}Native | {{/isNative}}  {{#isUnion}}{{#union}} [{{name}}](#{{name}}){{/union}}{{/isUnion}}{{#isObj}}<ul>{{#fields}} <li> **{{name}}**{{#type}}{{#isNative}}  [{{name}}](#{{name}}){{/isNative}}{{#isObj}} [{{name}}](#{{name}}){{/isObj}}{{#isUnion}}{{#union}} [{{name}}](#{{name}}){{/union}}{{/isUnion}} {{#hasParam}} [{{name}}](#{{name}})[{{#param}} [{{name}}](#{{name}}){{/param}}]{{/hasParam}}{{/type}}</li>{{/fields}}</ul>| {{/isObj}}
{{/types}}

# Input variables
|vars|type|doc|
|-------|---|---|
{{#vars}}| {{name}}|{{#type}}{{#isNative}} [{{name}}](#{{name}}) |{{/isNative}}{{#isObj}}  [{{name}}](#{{name}})| {{/isObj}}{{#isUnion}}{{#union}} [{{name}}](#{{name}}){{/union}}| {{/isUnion}}{{#hasParam}} {{name}}[{{#param}}[{{name}}](#{{name}}){{/param}}]{{/hasParam}}{{/type}} {{doc}}|
{{/vars}}


# Functions
|funcs|cost|doc|params|type|
|-------|-|---|---|---|
{{#funcs}}| {{name}}|{{cost}}|{{doc}}|<ul>{{#params}} <li>{{name}}{{#type}}{{#isComplex}} {{name}}{{/isComplex}}{{#isNative}} [{{name}}](#{{name}}){{/isNative}}{{#isObj}} [{{name}}](#{{name}}){{/isObj}}{{#isUnion}}{{#union}} [{{name}}](#{{name}}){{/union}}{{/isUnion}}{{#hasParam}} {{name}}[{{#param}}{{#needLink}} [{{name}}](#{{name}}){{/needLink}}{{#noLink}} {{name}}{{/noLink}}{{/param}}]{{/hasParam}}{{/type}} {{doc}}</li>{{/params}}</ul>|{{#type}}{{#isComplex}}  {{name}}{{/isComplex}}{{#isNative}} [{{name}}](#{{name}}){{/isNative}}{{#isObj}} [{{name}}](#{{name}}){{/isObj}}{{#isUnion}}{{#union}} [{{name}}](#{{name}}){{/union}}{{/isUnion}}{{#hasParam}} {{name}}[{{#param}}{{#needLink}} [{{name}}](#{{name}}){{/needLink}}{{#noLink}} {{name}}{{/noLink}}{{/param}}] {{/hasParam}}{{/type}}
{{/funcs}}

{{#commonFields}}
# Common fields
|tx type|id | fee| timestamp|version|sender|senderPublicKey|bodyBytes|proofs|
|---|---|---|---|---|---|---|---|---|
{{#types}}|{{name}}|{{#fields}}{{#absend}}-{{/absend}}{{#type}} {{#isNative}} [{{name}}](#{{name}}){{/isNative}}{{#isObj}} [{{name}}](#{{name}}){{/isObj}}{{#isUnion}}{{#union}} [{{name}}](#{{name}}){{/union}}{{/isUnion}} {{#hasParam}}{{name}}[{{#param}}[{{name}}](#{{name}}){{/param}}]{{/hasParam}}|{{/type}}{{/fields}}
{{/types}}
{{/commonFields}}


{{#specials}}
 <h1>{{class}} fields</h1>{{#descr}}<table><tr><td></td>{{#types}}<td>{{name}}</td>{{/types}}<tr>{{#fields}}<tr><td>{{name}}</td>{{#types}}<td>{{#absend}}-{{/absend}}{{#type}} {{#isNative}} <a href="#{{name}}">{{name}}</a>{{/isNative}}{{#isObj}} <a href="#{{name}}">{{name}}</a>{{/isObj}}{{#isUnion}}  {{#union}} <a href="#{{name}}">{{name}}</a>{{/union}}{{/isUnion}}{{#hasParam}} {{name}}[{{#param}}<a href="#{{name}}">{{name}}</a>{{/param}}]{{/hasParam}}{{/type}}</td>{{/types}}</tr>{{/fields}}</table>
{{/descr}}
{{/specials}}