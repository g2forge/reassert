package com.g2forge.reassert.contract.algorithm.licenseusage.model;

import java.util.List;

import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.name.LicenseTermLicenseUsageName;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.name.UsageTermLicenseUsageName;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.name.ILicenseUsageName;
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
public class LicenseUsageOperation implements IOperation<ILicenseUsageName, TermRelation> {
	public static class LicenseUsageOperationBuilder implements IOperationBuilder<ILicenseUsageName, TermRelation, LicenseUsageOperationBuilder, LicenseUsageOperation> {
		public LicenseUsageOperationBuilder argument$V(ILicenseTerm term) {
			return argument$V(new LicenseTermLicenseUsageName(term));
		}

		public LicenseUsageOperationBuilder argument$V(IUsageTerm term) {
			return argument$V(new UsageTermLicenseUsageName(term));
		}
	}

	@SafeVarargs
	public static LicenseUsageOperation and(IExpression<ILicenseUsageName, TermRelation>... arguments) {
		return new LicenseUsageOperation(Operator.AND, arguments);
	}

	public static LicenseUsageOperation not(IExpression<ILicenseUsageName, TermRelation> argument) {
		return new LicenseUsageOperation(Operator.NOT, argument);
	}

	public static LicenseUsageOperation not(ILicenseTerm term) {
		return not(of(term));
	}

	public static LicenseUsageOperation not(IUsageTerm term) {
		return not(of(term));
	}

	public static Variable<ILicenseUsageName, TermRelation> of(final ILicenseTerm term) {
		return new Variable<>(new LicenseTermLicenseUsageName(term));
	}

	public static Variable<ILicenseUsageName, TermRelation> of(final IUsageTerm term) {
		return new Variable<>(new UsageTermLicenseUsageName(term));
	}

	@SafeVarargs
	public static LicenseUsageOperation or(IExpression<ILicenseUsageName, TermRelation>... arguments) {
		return new LicenseUsageOperation(Operator.OR, arguments);
	}

	protected final BooleanOperation.Operator operator;

	@Singular
	protected final List<IExpression<ILicenseUsageName, TermRelation>> arguments;

	@SafeVarargs
	public LicenseUsageOperation(BooleanOperation.Operator operator, IExpression<ILicenseUsageName, TermRelation>... arguments) {
		this(operator, HCollection.asList(arguments));
	}
}
