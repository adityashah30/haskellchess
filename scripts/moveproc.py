#!/usr/bin/env python

global counter
global list_
global tree_str

class Node(object):

    def __init__(self, idno, move, parent):
        self.b = move[0]
        self.w = move[1]
        self.idno = idno
        self.parent = parent
        self.children = []

    def __str__(self):
        return '('+str(self.idno)+','+str(self.parent)+',"'+self.b+'","'+self.w+'")'

    def __repr__(self):
        return self.__str__()

    def __eq__(self, other):
        return (self.b == other.b and self.w == other.w)

    def check_membership(self, move):
        for i in self.children:
            if move[0] == i.b and move[1] == i.w:
                return i
        global counter
        global tree_str
        node = Node(counter, move, self.idno)
        tree_str += (str(node)+",\n")
        counter += 1
        self.children.append(node)
        return node

def print_tree(root):
    q = []
    tree_str=""
    dollar_node = Node(-1, ["$", "$"], None)
    pipe_node = Node(-1, ["|", "|"], None)
    q.append(root)
    q.append(dollar_node)
    while len(q)>1:
        node = q.pop(0)
        if node == dollar_node:
            q.append(dollar_node)
            tree_str += "\n----------\n"
            continue
        elif node == pipe_node:
            tree_str += str(pipe_node)
            continue
        tree_str += str(node) + " "
        q.extend(node.children)
        q.append(pipe_node)
    return tree_str

def sanitize_data(olddata):
    data = []
    for game in olddata:
        moves = ["NULL"]
        for move in game:
            moves.extend(move.split())
        del moves[len(moves)-1]
        newgame = []
        moveiter = iter(moves)
        while True:
            try:
                a, b = moveiter.next(), moveiter.next()
                newgame.append(" ".join([a, b]))
            except StopIteration:
                break
        data.append(newgame)
    return data

def main():
    with open('../data/openingbook.txt') as fp:
        data = sanitize_data([[j.strip() for j in i.strip().split("\n")] for i in fp.read().split(";") if i])
    tree = Node(-1, ['0', '0'], None)
    global counter
    global tree_str
    tree_str = "["
    counter = 0
    for game in data:
        root = tree
        for move in game:
            move = move.split()
            root = root.check_membership(move)
    tree_str = (tree_str[:-2]+"]")
    print tree_str
    with open('../data/opening_book_tree.dat', 'w') as fp:
        fp.write(tree_str)


if __name__ == "__main__":
    main()
