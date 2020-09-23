package com.g2forge.reassert.contract.v2.model.licenseusage;

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
public class CTOperation implements IOperation<ICTName, TermRelation> {
	public static class CTOperationBuilder implements IOperationBuilder<ICTName, TermRelation, CTOperationBuilder, CTOperation> {
		public CTOperationBuilder argument$V(ITerm term) {
			return argument$V(new CTNameType(term));
		}
	}

	@SafeVarargs
	public static CTOperation and(IExpression<ICTName, TermRelation>... arguments) {
		return new CTOperation(Operator.AND, arguments);
	}

	public static CTOperation not(IExpression<ICTName, TermRelation> argument) {
		return new CTOperation(Operator.NOT, argument);
	}

	public static CTOperation not(ITerm term) {
		return not(of(term));
	}

	public static Variable<ICTName, TermRelation> of(final ITerm term) {
		return new Variable<>(new CTNameType(term));
	}

	@SafeVarargs
	public static CTOperation or(IExpression<ICTName, TermRelation>... arguments) {
		return new CTOperation(Operator.OR, arguments);
	}

	protected final BooleanOperation.Operator operator;

	@Singular
	protected final List<IExpression<ICTName, TermRelation>> arguments;

	@SafeVarargs
	public CTOperation(BooleanOperation.Operator operator, IExpression<ICTName, TermRelation>... arguments) {
		this(operator, HCollection.asList(arguments));
	}
}
