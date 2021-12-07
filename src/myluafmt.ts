import { ThemeIcon } from "vscode"

const lustils = require("lustils")

const { parse, defaultOptions } = require("luaparse")

const { read, writer, writeBeautifiedText } = require("luon")

const commentWriter = writer({
	string_format: "long_newline"
})
const writeLongText = (content: any) => commentWriter.write(content).text

defaultOptions.ranges = true

const _precedency = [["^"], ["unary"], ["*", "/", "%"], ["+", "-"], [".."], ["<", ">", "<=", ">=", "~=", "=="], ["and"], ["or"]]

const precedency: any = {}
for (let i = 0; i < _precedency.length; i++) {
	for (let operator of _precedency[i]) precedency[operator] = _precedency.length - i
}

const commentParents: any = {
	Chunk: "body",
	DoStatement: "body",
	FunctionDeclaration: "body",
	IfClause: "body",
	ElseifClause: "body",
	ElseClause: "body",
	WhileStatement: "body",
	RepeatStatement: "body",
	ForNumericStatement: "body",
	ForGenericStatement: "body",
	TableConstructorExpression: "fields"
}

// HACK to make luaparse ranges be from "if" to "end" and not from "if" to "then" for IfClauses
// TODO ensure luaparse behaves strangely only for IfClauses
const fixRanges = (node: { [x: string]: any; range: any[]; type: string; clauses: string | any[] }) => {
	if (node.range) {
		if (node.type === "IfStatement") {
			for (let i = 0; i < node.clauses.length - 1; i++) {
				node.clauses[i].range[1] = node.clauses[i + 1].range[0] - 1
			}
			node.clauses[node.clauses.length - 1].range[1] = node.range[1]
		}
	}
	for (let key in node) {
		let children = node[key]
		if (Array.isArray(children)) children.forEach(fixRanges)
		else if (children !== null && typeof children === "object" && children.type) fixRanges(children)
	}
}

const insertComment = (node: any, comment: { range: number[] }) => {
	for (let key in node) {
		let children = node[key]
		if (Array.isArray(children)) {
			// TODO binary search, ensure AST is properly sorted for this to work
			let comment_parent_index = children.findIndex(
				node => node.range && node.range[0] <= comment.range[0] && node.range[1] >= comment.range[1]
			)
			const comment_parent = comment_parent_index !== -1 && children[comment_parent_index]
			if (comment_parent && insertComment(comment_parent, comment)) return true
			if (commentParents[node.type] === key) {
				if (children.length) {
					comment_parent_index = comment_parent
						? comment_parent_index
						: children.findIndex(node => node.range[0] > comment.range[1])
					if (comment_parent_index === -1) comment_parent_index = children.length + 1
					// insert comment as own child
					node[key].splice(comment_parent_index, 0, comment)
				} else {
					node[key] = [comment]
				}
				return true
			}
		} else if (children !== null && typeof children === "object" && children.type) {
			if (insertComment(children, comment)) {
				return true
			}
		}
	}
}

const insertComments = (chunk: { comments: any[] }) => chunk.comments.forEach((comment: any) => insertComment(chunk, comment))

const isBinaryExpression = (node: { type: string }) => node.type === "LogicalExpression" || node.type === "BinaryExpression"

const indexNoParens: any = {
	Identifier: true,
	MemberExpression: true,
	IndexExpression: true,
	CallStatement: true,
	CallExpression: true,
	TableCallExpression: true,
	StringCallExpression: true
}

const default_conf = {
	indent: "\t",
	newline: "\n",
	extra_newlines: true,
	enable_simple_calls: false,
	inline: {
		block: {
			max_exp_length: 60
		},
		table: {
			max_field_count: 3,
			max_field_length: 60
		}
	}
}
Object.freeze(default_conf)

const assertType = (value: any, type: string) => {
	if (typeof value !== type) throw new TypeError("invalid type")
}

const formatter = (conf?: any) => {
	assertType((conf = conf === undefined ? {} : conf), "object")
	conf = lustils.object.complete(conf, default_conf)
	let { indent, newline, extra_newlines, enable_simple_calls, inline } = conf
	if (!/^\s*$/.test(indent)) throw new Error('"indent" needs to be a string of spacing characters')
	const indentationText = (number: any) => indent.repeat(number)
	const escape_list: any = { "\n": true, "\r": true, "\r\n": true }
	if (!escape_list[newline])
		throw new Error('"newline" needs to be CR ("\\r"), LF ("\\n"), or CRLF ("\\r\\n")')
	const indentationNewline = (number?: number) => newline + indentationText(number)
	assertType(extra_newlines, "boolean")
	assertType(inline, "object")
	inline = lustils.object.complete(inline, default_conf.inline)
	let inline_block = inline.block
	let inline_table = inline.table
	if (inline_block !== false) {
		assertType(inline_block, "object")
		inline_block = lustils.object.complete(inline_block, default_conf.inline.block)
		assertType(inline_block.max_exp_length, "number")
	}
	if (inline_table !== false) {
		assertType(inline_table, "object")
		inline_table = lustils.object.complete(inline_table, default_conf.inline.table)
		assertType(inline_table.max_field_count, "number")
		assertType(inline_table.max_field_length, "number")
	}

	const block = (fnc: any, formatted_body_consumer?: boolean, trailing_spaces?: boolean) => {
		trailing_spaces = trailing_spaces || !formatted_body_consumer
		return (node: { body: any }, indent: number) => {
			const { body } = node
			const body_length = body.length
			const body_pp = prettyPrint(body, indent + 1)
			let body_formatted
			if (body_length === 0) body_formatted = " "
			else if (
				body_length === 1 &&
				inline_block &&
				body[0].type !== "Comment" &&
				body_pp.length <= inline_block.max_exp_length &&
				!body_pp.includes(newline)
			)
				body_formatted = " " + body_pp + (trailing_spaces ? " " : "")
			else body_formatted = indentationNewline(indent + 1) + body_pp + (trailing_spaces ? indentationNewline(indent) : "")
			return !formatted_body_consumer ? fnc(node, indent) + body_formatted + "end" : fnc(node, indent, body_formatted)
		}
	}

	const mapPrettyPrint: any = (array: any[], indent: any) => array.map((node: any) => prettyPrint(node, indent))
	const mapPrettyPrintJoin = (array: any, indent?: any) => mapPrettyPrint(array, indent).join(", ")

	const indexPrettyPrint = (node: any, indent: any) => {
		const { base } = node
		const base_formatted = prettyPrint(base, indent)
		return indexNoParens[base.type] ? base_formatted : "(" + base_formatted + ")"
	}

	const AssignmentStatement = (node: any, indent: any) =>
		mapPrettyPrintJoin(node.variables, indent) + (node.init.length ? " = " + mapPrettyPrintJoin(node.init, indent) : "")

	const generateClauseFormatters = (trailing_spaces?: boolean) => {
		const IfClause = block(
			function (node: any, indent: any, body_formatted: string): any {
				return "if " + prettyPrint(node.condition, indent) + " then" + body_formatted
			},
			true,
			trailing_spaces
		)
		const ElseifClause = (node: any, indent: any) => "else" + IfClause(node, indent)
		const ElseClause = block((_node: any, _indent: any, body_formatted: string) => "else" + body_formatted, true, true)
		return {
			IfClause,
			ElseifClause,
			ElseClause
		}
	}

	const clauseFormatters: any = generateClauseFormatters()
	const clauseFormattersTrailingSpaces: any = generateClauseFormatters(true)

	const BinaryExpression = (node: { left: any; right: any; operator: any }, indent: any) => {
		const { left, right, operator } = node
		const op_precedency = precedency[operator]
		let left_pp = prettyPrint(left, indent)
		let right_pp = prettyPrint(right, indent)
		if (isBinaryExpression(left) && precedency[left.operator] < op_precedency) left_pp = "(" + left_pp + ")"
		if (
			(isBinaryExpression(right) && precedency[right.operator] < op_precedency) ||
			(right.operator !== ".." && precedency[right.operator] == op_precedency)
		)
			right_pp = "(" + right_pp + ")"
		return left_pp + " " + operator + " " + right_pp
	}

	let formatters: any = {
		// comments
		Comment: (node: { value: string; raw: string }, indent: number) => {
			let content = node.value.trim()
			if (!node.raw.startsWith("--[") || !content.includes(newline)) return "-- " + content.trim()
			return (
				"--" +
				writeLongText(
					indentationText(indent + 1) +
					content.replace(/\s*(\r?\n|\r)\s*/g, indentationNewline(indent + 1)) +
					indentationNewline(indent)
				)
			)
		},

		// various trivial stuff; identifiers, simple statements
		Identifier: (node: { name: any }) => node.name,
		Chunk: function (node: { body: any }, indent: any) {
			return prettyPrint(node.body, indent)
		},
		LabelStatement: (node: { label: string }) => "::" + node.label + "::",
		GotoStatement: (node: { label: string }) => "goto " + node.label,
		BreakStatement: (_: any) => "break",
		ReturnStatement: (node: { arguments: string | any[] }, indent: any) =>
			node.arguments.length === 0 ? "return" : "return " + mapPrettyPrintJoin(node.arguments, indent),
		DoStatement: block(() => "do"),

		// assignments
		AssignmentStatement,
		LocalStatement: (node: any, indent: any) => "local " + AssignmentStatement(node, indent),
		FunctionDeclaration: block(
			(node: { isLocal: any; identifier: any; parameters: any }) =>
				(node.isLocal ? "local " : "") +
				"function" +
				(node.identifier ? " " + prettyPrint(node.identifier) : "") +
				"(" +
				mapPrettyPrintJoin(node.parameters) +
				")"
		),

		// if-[elseif]-[else] chains
		IfStatement: (node: { clauses: any }, indent: any) => {
			const clauses = node.clauses
			let out = ""
			const prev_ind = indentationNewline(indent)
			for (let i = 0; i < clauses.length; i++) {
				const clause = clauses[i]
				if (i !== 0) out += prev_ind
				const tmp = (i === clauses.length - 1 ? clauseFormattersTrailingSpaces : clauseFormatters)
				out += tmp[clause.type](clause, indent)
			}
			return out + "end"
		},

		// loops
		WhileStatement: block((node: { condition: any }, indent: any) => "while " + prettyPrint(node.condition, indent) + " do"),
		RepeatStatement: block(
			(node: { condition: any }, indent: any, body_formatted: string) => "repeat" + body_formatted + "until " + prettyPrint(node.condition, indent),
			true,
			true
		),
		ForNumericStatement: block(
			(node: { variable: any; start: any; end: any; step: any }) =>
				"for " + prettyPrint(node.variable) + " = " + mapPrettyPrintJoin([node.start, node.end, node.step].filter(x => x)) + " do"
		),
		ForGenericStatement: block(
			(node: { variables: any; iterators: any }, indent: any) => "for " + mapPrettyPrintJoin(node.variables) + " in " + mapPrettyPrintJoin(node.iterators, indent) + " do"
		),

		// operators / expressions / calls
		UnaryExpression: (node: { operator: any; argument: any }, indent: any) => {
			const { operator, argument } = node
			let argument_pp = prettyPrint(argument, indent)
			if (isBinaryExpression(argument) && precedency[argument.operator] < precedency.unary) argument_pp = "(" + argument_pp + ")"
			else if (operator === "not" || argument.type === "UnaryExpression") argument_pp = " " + argument_pp
			return operator + argument_pp
		},
		BinaryExpression,
		LogicalExpression: BinaryExpression,
		CallStatement: (node: { expression: any }, indent: any) => prettyPrint(node.expression, indent),
		CallExpression: (node: { arguments: string | any[]; base: any }, indent: any) => {
			if (enable_simple_calls && node.arguments.length === 1) {
				const argument = node.arguments[0]
				if (argument.type === "StringLiteral")
					return formatters.StringCallExpression(
						{
							base: node.base,
							argument: argument
						},
						indent
					)
				if (argument.type === "TableConstructorExpression")
					return formatters.TableCallExpression(
						{
							base: node.base,
							arguments: argument
						},
						indent
					)
			}

			if (node.base != undefined && node.base.type === "FunctionDeclaration") {

				return "(" + prettyPrint(node.base, indent) + ")(" + mapPrettyPrintJoin(node.arguments, indent) + ")"
			}

			return prettyPrint(node.base, indent) + "(" + mapPrettyPrintJoin(node.arguments, indent) + ")"
		},

		StringCallExpression: (node: { base: any; argument: any }, indent: any) => prettyPrint(node.base, indent) + prettyPrint(node.argument),
		TableCallExpression: (node: { base: any; arguments: any }, indent: any) => prettyPrint(node.base, indent) + prettyPrint(node.arguments, indent),

		TableKey: (node: { key: any; value: any }, indent: any) => "[" + prettyPrint(node.key, indent) + "] = " + prettyPrint(node.value, indent),
		TableKeyString: (node: { key: any; value: any }, indent: any) => prettyPrint(node.key, indent) + " = " + prettyPrint(node.value, indent),
		TableValue: (node: { value: any }, indent: any) => prettyPrint(node.value, indent),
		TableConstructorExpression: (node: { fields: any[] }, indent: number) => {
			const length = node.fields.length
			if (length === 0) return "{}"
			indent++
			const fields_pp = mapPrettyPrint(node.fields, indent)
			const inline =
				inline_table &&
				length <= inline_table.max_field_count &&
				!node.fields.find((field: { type: string }) => field.type === "Comment") &&
				!fields_pp.find((formatted: string | any[]) => formatted.length > inline_table.max_field_length || formatted.includes(newline))
			const spacing = inline ? " " : indentationNewline(indent)
			const end_spacing = inline ? " " : indentationNewline(indent - 1)
			let table = [end_spacing + "}"]
			let beforeField = false
			for (let i = length - 1; i > -1; i--) {
				const field = node.fields[i]
				const isField = field.type !== "Comment"
				if (beforeField && isField) {
					table.push(",")
					beforeField = false
				}
				table.push(spacing + prettyPrint(field, indent))
				beforeField = beforeField || isField
			}
			table.push("{")
			return table.reverse().join("")
		},
		MemberExpression: (node: { indexer: any; identifier: any }, indent: any) => indexPrettyPrint(node, indent) + node.indexer + prettyPrint(node.identifier, indent),
		IndexExpression: (node: { index: any }, indent: any) => indexPrettyPrint(node, indent) + "[" + prettyPrint(node.index, indent) + "]",

		NilLiteral: (node: { raw: any }) => node.raw,
		VarargLiteral: (node: { raw: any }) => node.raw,
		BooleanLiteral: (node: { raw: any }) => node.raw,
		StringLiteral: (node: { value: any; raw: any }) => writeBeautifiedText(read(node.value || node.raw)),
		NumericLiteral: (node: { value: any; raw: string }) => {
			node.value = node.value || read(node.raw)
			const rawUpper = node.raw.toUpperCase()
			if (rawUpper.startsWith("0X")) return "0x" + rawUpper.substring(2)
			return writeBeautifiedText(node.value)
		}
	}

	const extraNewlines: any = {
		FunctionDeclaration: true
	}

	const prettyPrint = (node: any, indent?: number) => {
		if (Array.isArray(node)) {
			const indentNewline = indentationNewline(indent)
			if (!extra_newlines) return mapPrettyPrint(node, indent).join(indentNewline)
			let formatted = ""
			let extraNewline
			let i = 0
			for (; i < node.length; i++) {
				const notFirst = i > 0
				let comments = []
				let j = i
				for (; j < node.length && node[j].type === "Comment"; j++) comments.push(prettyPrint(node[j], indent))
				if (i + comments.length === node.length) {
					if (notFirst) formatted += indentNewline
					formatted += comments.join(indentNewline)
					break
				}
				i = j
				const child = node[i]
				const prevExtraNewline = extraNewline
				extraNewline = extraNewlines[child.type]
				if (notFirst) formatted += (prevExtraNewline || extraNewline ? newline : "") + indentNewline
				if (comments.length > 0) {
					formatted += comments.join(indentNewline)
					formatted += indentNewline
				}
				formatted += prettyPrint(child, indent)
			}
			return formatted
		}
		const nodeFormatter = formatters[node.type]
		if (!nodeFormatter) throw new Error(`Formatter for ${node.type} not implemented yet`)
		return nodeFormatter(node, indent)
	}

	return prettyPrint
}

function astFormatter(conf: any) {
	return (ast: any, indent?: any) => {
		return formatter(conf)(ast, indent === undefined ? 0 : indent)
	}
}

export const textFormatter = (conf?: any) => {
	const formatter = astFormatter(conf)
	return (text: any) => {
		const ast = parse(text)
		fixRanges(ast)
		insertComments(ast)
		return formatter(ast)
	}
}

export const ast = { fixRanges, insertComments, formatter: astFormatter }
export const formatChunk = textFormatter()

// module.exports = {
// 	ast: { fixRanges, insertComments, formatter: astFormatter },
// 	formatChunk: textFormatter(),
// 	formatter: textFormatter
// }
