package com.g2forge.reassert.contract.algorithm.licenseusage.model.rule;

import com.g2forge.alexandria.java.function.builder.IBuilder;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.name.ILicenseUsageName;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.name.LicenseTermLicenseUsageName;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.name.UsageTermLicenseUsageName;
import com.g2forge.reassert.contract.model.finding.IFindingFactory;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.operation.BooleanOperation;
import com.g2forge.reassert.express.model.variable.Variable;

public interface ILicenseUsageRule {
	public interface ILicenseUsageRuleBuilder<Builder extends ILicenseUsageRuleBuilder<Builder, Built>, Built extends ILicenseUsageRule> extends IBuilder<Built> {
		public Builder expression(IExpression<ILicenseUsageName, TermRelation> expression);

		public Builder finding(IFindingFactory<?> finding);

		public default IExpression<ILicenseUsageName, TermRelation> not(ILicenseTerm term) {
			return BooleanOperation.not(of(term));
		}

		public default IExpression<ILicenseUsageName, TermRelation> not(IUsageTerm term) {
			return BooleanOperation.not(of(term));
		}

		public default IExpression<ILicenseUsageName, TermRelation> of(final ILicenseTerm term) {
			return new Variable<>(new LicenseTermLicenseUsageName(term));
		}

		public default IExpression<ILicenseUsageName, TermRelation> of(final IUsageTerm term) {
			return new Variable<>(new UsageTermLicenseUsageName(term));
		}
	}

	public IExpression<ILicenseUsageName, TermRelation> getExpression();

	public IFindingFactory<?> getFinding();
}
