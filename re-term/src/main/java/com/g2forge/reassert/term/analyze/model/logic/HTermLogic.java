package com.g2forge.reassert.term.analyze.model.logic;

import java.util.stream.Collectors;

import com.g2forge.alexandria.analysis.HAnalysis;
import com.g2forge.alexandria.analysis.ISerializableFunction1;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.function.IFunction2;
import com.g2forge.alexandria.java.type.function.TypeSwitch2;
import com.g2forge.reassert.core.model.contract.ITerm;
import com.g2forge.reassert.core.model.contract.TermRelation;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.term.eee.express.IExpression;
import com.g2forge.reassert.term.eee.express.Literal;
import com.g2forge.reassert.term.eee.express.Operation;

import lombok.AccessLevel;
import lombok.Getter;

public class HTermLogic {
	@Getter(lazy = true, value = AccessLevel.PUBLIC)
	private static final IFunction2<? super ITermLogicContext, ? super ITerm, ? extends IExpression<TermRelation>> compiler = computeCompiler();

	public static ITerm and(ITerm... arguments) {
		return new OperationTerm(Operation.Operator.AND, arguments);
	}

	protected static IFunction2<? super ITermLogicContext, ? super ITerm, ? extends IExpression<TermRelation>> computeCompiler() {
		final TypeSwitch2.FunctionBuilder<ITermLogicContext, ITerm, IExpression<TermRelation>> builder = new TypeSwitch2.FunctionBuilder<>();
		builder.add(ITermLogicContext.class, ILicenseTerm.class, ITermLogicContext::term);
		builder.add(ITermLogicContext.class, IUsageTerm.class, ITermLogicContext::term);
		builder.add(ITermLogicContext.class, OperationTerm.class, (c, t) -> {
			final IFunction1<? super ITerm, ? extends IExpression<TermRelation>> compile = getCompiler().curry0(c);
			return new Operation<>(t.getOperator(), t.getArguments().stream().map(compile).collect(Collectors.toList()));
		});
		builder.add(ITermLogicContext.class, ContextTerm.class, (c, t) -> {
			@SuppressWarnings("unchecked")
			final IFunction1<Object, ? extends TermRelation> function = (IFunction1<? super Object, ? extends TermRelation>) t.getFunction();
			return new Literal<>(t.getName(), function.apply(c.getContext()));
		});
		return builder.build();
	}

	public static <I, T> ITerm context(ISerializableFunction1<? super I, ? extends T> accessor, IFunction1<? super T, ? extends TermRelation> adapter) {
		return new ContextTerm(HAnalysis.getPath(accessor), accessor.andThen(adapter));
	}

	public static ITerm not(ITerm term) {
		return new OperationTerm(Operation.Operator.NOT, term);
	}

	public static ITerm or(ITerm... arguments) {
		return new OperationTerm(Operation.Operator.OR, arguments);
	}
}
