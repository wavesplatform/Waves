# {{category}}

|#|Name|Description|Complexity|
| :--- | :--- | :--- | :--- |
{{#funcs}}|{{index}}|[{{name}}({{{paramTypes}}}): {{#type}}{{{mdName}}}{{/type}}](#{{anchor}})|{{{doc}}}|{{cost}}|
{{/funcs}}
{{#funcs}}

## {{name}}({{{paramTypes}}}): {{#type}}{{{mdName}}}{{/type}}<a id = "{{anchor}}"></a>
{{{doc}}}

``` ride
{{name}}({{{paramArgTypes}}}): {{#type}}{{name}}{{/type}}
```

### Parameters
{{#params}}

#### `{{name}}`: {{#type}}{{{mdName}}}{{/type}}
{{{doc}}}
{{/params}}
{{/funcs}}