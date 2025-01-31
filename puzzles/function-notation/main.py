import ast


class SwapBinOpTransformer(ast.NodeTransformer):
    def visit_BinOp(self, node):
        # Only swap Mult (~ do not modify Add)
        if isinstance(node, ast.BinOp) and isinstance(node.op, ast.Mult):
            node.left, node.right = node.right, node.left
        return self.generic_visit(node)


class Unparser(ast._Unparser):  # type: ignore
    """https://github.com/python/cpython/blob/e3eba8ce266f90d9f8faeb5b2b4b64e56110bd2a/Lib/ast.py#L666"""

    def require_parens(self, precedence, node):
        return self.delimit_if(
            "(",
            ")",
            self.get_precedence(node) > precedence
            # We add this line so that:
            #   f . g . f . h
            # becomes:
            #   h |> f |> g |> f
            # instead of:
            #   h |> (f |> (g |> f))
            and not isinstance(node.op, ast.Mult),
        )


def unparse(ast_obj):
    unparser = Unparser()
    return unparser.visit(ast_obj)


def main():
    function = input().replace(".", "*")
    tree = ast.parse(function)
    transformer = SwapBinOpTransformer()
    new_tree = transformer.visit(tree)
    new_function = unparse(new_tree).replace("*", "|>")
    print(new_function)


if __name__ == "__main__":
    main()
