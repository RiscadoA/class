package pt.inescid.cllsj.compiler;

import java.util.HashMap;
import java.util.Map;

import pt.inescid.cllsj.ast.ASTNodeVisitor;
import pt.inescid.cllsj.ast.nodes.ASTBang;
import pt.inescid.cllsj.ast.nodes.ASTCall;
import pt.inescid.cllsj.ast.nodes.ASTCase;
import pt.inescid.cllsj.ast.nodes.ASTClose;
import pt.inescid.cllsj.ast.nodes.ASTCoClose;
import pt.inescid.cllsj.ast.nodes.ASTCut;
import pt.inescid.cllsj.ast.nodes.ASTEmpty;
import pt.inescid.cllsj.ast.nodes.ASTExpr;
import pt.inescid.cllsj.ast.nodes.ASTFwd;
import pt.inescid.cllsj.ast.nodes.ASTMix;
import pt.inescid.cllsj.ast.nodes.ASTNode;
import pt.inescid.cllsj.ast.nodes.ASTRecv;
import pt.inescid.cllsj.ast.nodes.ASTSelect;
import pt.inescid.cllsj.ast.nodes.ASTSend;
import pt.inescid.cllsj.ast.nodes.ASTWhy;

public class SessionRenamer extends ASTNodeVisitor {
    // How many different occurrences of each session name have been found
    private Map<String, Integer> occurrences = new HashMap<>();

    public static void execute(ASTNode node) {
        node.accept(new SessionRenamer());
    }

    private String introduce(String session) {
        Integer occurences = this.occurrences.compute(session, (k, v) -> v == null ? 1 : v + 1);
        return session + "_" + occurences;
    }

    private String rename(String session) {
        Integer occurences = this.occurrences.get(session);
        assert occurences != null : "Session " + session + " was not introduced";
        return session + "_" + occurences;
    }

    @Override
    public void visit(ASTNode node) {
        throw new UnsupportedOperationException(
            "Renaming of sessions in nodes of type " + node.getClass().getSimpleName() + " is not yet supported");
    }

    @Override
    public void visit(ASTBang node) {
        node.setChr(rename(node.getChr()));
        node.setChi(introduce(node.getChi()));
        node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTCall node) {
        node.setChr(rename(node.getChr()));
        node.setChi(introduce(node.getChi()));
        node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTCase node) {
        node.setCh(rename(node.getCh()));
        for (ASTNode branch : node.getCases().values()) {
            branch.accept(this);
        }
    }

    @Override
    public void visit(ASTClose node) {
        node.setCh(rename(node.getCh()));
    }

    @Override
    public void visit(ASTCoClose node) {
        node.setCh(rename(node.getCh()));
        node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTCut node) {
        node.setCh(introduce(node.getCh()));
        node.getLhs().accept(this);
        node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTEmpty node) {}

    @Override
    public void visit(ASTExpr node) {}

    @Override
    public void visit(ASTFwd node) {
        node.setCh1(rename(node.getCh1()));
        node.setCh2(rename(node.getCh2()));
    }

    @Override
    public void visit(ASTMix node) {
        node.getLhs().accept(this);
        node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTRecv node) {
        node.setChr(rename(node.getChr()));
        node.setChi(introduce(node.getChi()));
        node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTSelect node) {
        node.setCh(rename(node.getCh()));
        node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTSend node) {
        node.setChs(rename(node.getChs()));
        node.setCho(introduce(node.getCho()));
        node.getLhs().accept(this);
        node.getRhs().accept(this);
    }

    @Override
    public void visit(ASTWhy node) {
        node.setCh(rename(node.getCh()));
        node.getRhs().accept(this);
    }
}
