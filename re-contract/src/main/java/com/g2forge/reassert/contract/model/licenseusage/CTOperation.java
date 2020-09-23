package com.g2forge.reassert.contract.model.licenseusage;

import java.util.List;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.operation.BooleanOperation;
import com.g2forge.reassert.express.model.operation.BooleanOperation.Operator;
import com.g2forge.reassert.express.model.operation.IOperation;
import com.g2forge.reassert.express.model.variable.Variable;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.Singular;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class CTOperation implements IOperation<ICTName, TermRelation> {
	public static class CTOperationBuilder implements IOperationBuilder<ICTName, TermRelation, CTOperationBuilder, CTOperation> {
		public CTOperationBuilder argument$V(ILicenseTerm term) {
			return argument$V(new CTNameLicenseTerm(term));
		}

		public CTOperationBuilder argument$V(IUsageTerm term) {
			return argument$V(new CTNameUsageTerm(term));
		}
	}

	@SafeVarargs
	public static CTOperation and(IExpression<ICTName, TermRelation>... arguments) {
		return new CTOperation(Operator.AND, arguments);
	}

	public static CTOperation not(IExpression<ICTName, TermRelation> argument) {
		return new CTOperation(Operator.NOT, argument);
	}

	public static CTOperation not(ILicenseTerm term) {
		return not(of(term));
	}

	public static CTOperation not(IUsageTerm term) {
		return not(of(term));
	}

	public static Variable<ICTName, TermRelation> of(final ILicenseTerm term) {
		return new Variable<>(new CTNameLicenseTerm(term));
	}

	public static Variable<ICTName, TermRelation> of(final IUsageTerm term) {
		return new Variable<>(new CTNameUsageTerm(term));
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
