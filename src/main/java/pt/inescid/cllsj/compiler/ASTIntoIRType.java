package pt.inescid.cllsj.compiler;

import java.util.ArrayList;
import java.util.List;

import pt.inescid.cllsj.ast.ASTTypeVisitor;
import pt.inescid.cllsj.ast.types.ASTBotT;
import pt.inescid.cllsj.ast.types.ASTCaseT;
import pt.inescid.cllsj.ast.types.ASTCoRecT;
import pt.inescid.cllsj.ast.types.ASTIdT;
import pt.inescid.cllsj.ast.types.ASTOfferT;
import pt.inescid.cllsj.ast.types.ASTOneT;
import pt.inescid.cllsj.ast.types.ASTRecT;
import pt.inescid.cllsj.ast.types.ASTRecvT;
import pt.inescid.cllsj.ast.types.ASTSendT;
import pt.inescid.cllsj.ast.types.ASTType;
import pt.inescid.cllsj.compiler.ir.type.IRClose;
import pt.inescid.cllsj.compiler.ir.type.IRRec;
import pt.inescid.cllsj.compiler.ir.type.IRSession;
import pt.inescid.cllsj.compiler.ir.type.IRTag;
import pt.inescid.cllsj.compiler.ir.type.IRType;
import pt.inescid.cllsj.compiler.ir.type.IRVar;

public class ASTIntoIRType extends ASTTypeVisitor {
    private Environment env;
    private IRType ir;

    public static IRType convert(Environment env, ASTType type) {
        ASTIntoIRType converter = new ASTIntoIRType(env);
        type.accept(converter);
        return converter.ir;
    }

    private ASTIntoIRType(Environment env) {
        this.env = env;
    }

    @Override
    public void visit(ASTType type) {
        throw new UnsupportedOperationException("Type not supported by IntoIRType: " + type);
    }

    @Override
    public void visit(ASTOneT type) {
        ir = new IRClose();
    }

    @Override
    public void visit(ASTBotT type) {
        ir = new IRClose();
    }

    @Override
    public void visit(ASTSendT type) {
        ir = new IRSession(ASTIntoIRType.convert(env, type.getrhs()));
    }

    @Override
    public void visit(ASTRecvT type) {
        ir = new IRSession(ASTIntoIRType.convert(env, type.getrhs()));
    }

    @Override
    public void visit(ASTCaseT type) {
        List<IRType> choices = new ArrayList<>();
        for (int i = 0; i < type.getcases().size(); i++) {
            choices.add(new IRSession(ASTIntoIRType.convert(env, type.getCaseType(type.getLabel(i)))));
        }
        ir = new IRTag(choices);
    }

    @Override
    public void visit(ASTOfferT type) {
        List<IRType> choices = new ArrayList<>();
        for (int i = 0; i < type.getcases().size(); i++) {
            choices.add(new IRSession(ASTIntoIRType.convert(env, type.getCaseType(type.getLabel(i)))));
        }
        ir = new IRTag(choices);
    }

    @Override
    public void visit(ASTRecT type) {
        ir = new IRRec(ASTIntoIRType.convert(env, type.getin()));
    }

    @Override
    public void visit(ASTCoRecT type) {
        ir = new IRRec(ASTIntoIRType.convert(env, type.getin()));
    }

    @Override
    public void visit(ASTIdT type) {
        ASTType unfolded;
        try {
            unfolded = type.unfoldType(env.getEp());
        } catch (Exception e) {
            throw new IllegalArgumentException("Error unfolding type: " + e.getMessage());
        }

        if (unfolded instanceof ASTIdT) {
            type = (ASTIdT) unfolded;
            ir = new IRVar(env.getTypeVarIndex(type.getid()));
        } else {
            unfolded.accept(this);
        }
    }
}
