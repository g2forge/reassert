package com.g2forge.reassert.contract.v2.model;

import java.util.List;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.reassert.core.model.contract.terms.ITerm;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.v2.model.IExpression;
import com.g2forge.reassert.express.v2.model.operation.BooleanOperation;
import com.g2forge.reassert.express.v2.model.operation.BooleanOperation.Operator;
import com.g2forge.reassert.express.v2.model.operation.IOperation;
import com.g2forge.reassert.express.v2.model.variable.Variable;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class TermOperation implements IOperation<ICTName, TermRelation> {
	public static class TermOperationBuilder implements IOperationBuilder<ICTName, TermRelation, TermOperationBuilder, TermOperation> {
		public TermOperationBuilder argument$V(ITerm term) {
			return argument$V(new CTNameType(term));
		}
	}

	@SafeVarargs
	public static TermOperation and(IExpression<ICTName, TermRelation>... arguments) {
		return new TermOperation(Operator.AND, arguments);
	}

	@SafeVarargs
	public static TermOperation or(IExpression<ICTName, TermRelation>... arguments) {
		return new TermOperation(Operator.OR, arguments);
	}

	public static TermOperation not(IExpression<ICTName, TermRelation> argument) {
		return new TermOperation(Operator.NOT, argument);
	}

	public static TermOperation not(ITerm term) {
		return not(of(term));
	}

	public static IExpression<ICTName, TermRelation> of(ITerm term) {
		return new Variable<>(new CTNameType(term));
	}

	protected final BooleanOperation.Operator operator;

	@Singular
	protected final List<IExpression<ICTName, TermRelation>> arguments;

	@SafeVarargs
	public TermOperation(BooleanOperation.Operator operator, IExpression<ICTName, TermRelation>... arguments) {
		this(operator, HCollection.asList(arguments));
	}
}
