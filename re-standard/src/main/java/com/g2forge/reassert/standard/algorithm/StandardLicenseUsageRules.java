package com.g2forge.reassert.standard.algorithm;

import static com.g2forge.reassert.express.model.operation.BooleanOperation.and;
import static com.g2forge.reassert.express.model.operation.BooleanOperation.not;
import static com.g2forge.reassert.express.model.operation.BooleanOperation.or;
import static com.g2forge.reassert.express.model.operation.BooleanOperation.implies;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.LicenseUsageNameScheme;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.finding.ConditionFinding;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.finding.CopyrightNoticeFinding;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.finding.DiscloseSourceFinding;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.finding.StateChangesFinding;
import com.g2forge.reassert.contract.model.IContractComparisonScheme;
import com.g2forge.reassert.contract.model.rule.AContractComparisonRules;
import com.g2forge.reassert.contract.model.rule.IContractComparisonRule;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.standard.model.contract.license.StandardLicenseTerm;
import com.g2forge.reassert.standard.model.contract.usage.StandardUsageTerm;
import com.g2forge.reassert.standard.model.contract.usage.StandardUsageTermAttribute;

import lombok.Getter;

@ReassertLegalOpinion
public class StandardLicenseUsageRules extends AContractComparisonRules<ILicenseTerm, IUsageTerm> implements ISingleton {
	protected static final StandardLicenseUsageRules INSTANCE = new StandardLicenseUsageRules();

	public static StandardLicenseUsageRules create() {
		return INSTANCE;
	}

	@Getter(lazy = true)
	private final List<IContractComparisonRule> rules = Collections.unmodifiableList(computeRules());

	protected StandardLicenseUsageRules() {}

	protected List<IContractComparisonRule> computeRules() {
		final List<IContractComparisonRule> rules = new ArrayList<>();

		// Usage terms
		rules.add(rule(b -> b.expression(implies(b.b(StandardUsageTerm.Commercial), b.a(StandardLicenseTerm.CommercialUse))).finding(ConditionFinding::new)));

		rules.add(rule(b -> b.expression(implies(b.b(StandardUsageTerm.DistributionPublic), b.a(StandardLicenseTerm.Distribution))).finding(ConditionFinding::new)));
		rules.add(rule(b -> b.expression(implies(b.b(StandardUsageTerm.DistributionPrivate), b.a(StandardLicenseTerm.PrivateUse))).finding(ConditionFinding::new)));
		rules.add(rule(b -> b.expression(or(b.notB(StandardUsageTerm.DistributionService), b.notA(StandardLicenseTerm.SaaSIsDistribution), b.a(StandardLicenseTerm.Distribution))).finding(ConditionFinding::new)));

		rules.add(rule(b -> b.expression(implies(b.b(StandardUsageTerm.UseLink), b.a(StandardLicenseTerm.PrivateUse))).finding(ConditionFinding::new)));
		rules.add(rule(b -> b.expression(implies(b.b(StandardUsageTerm.UseCopy), b.a(StandardLicenseTerm.PrivateUse))).finding(ConditionFinding::new)));
		rules.add(rule(b -> b.expression(implies(b.b(StandardUsageTerm.UseModified), and(b.a(StandardLicenseTerm.PrivateUse), b.a(StandardLicenseTerm.Modification)))).finding(ConditionFinding::new)));

		rules.add(rule(b -> b.expression(implies(b.b(StandardUsageTerm.DistributingBinary), b.a(StandardLicenseTerm.PrivateUse))).finding(ConditionFinding::new)));
		rules.add(rule(b -> b.expression(implies(b.b(StandardUsageTerm.DistributingSource), b.a(StandardLicenseTerm.PrivateUse))).finding(ConditionFinding::new)));

		// License conditions
		rules.add(rule(b -> b.expression(and(or(b.b(StandardUsageTerm.DistributionPublic), and(b.b(StandardUsageTerm.DistributionService), b.a(StandardLicenseTerm.SaaSIsDistribution))), b.a(StandardLicenseTerm.DisclosureSource))).finding(DiscloseSourceFinding::new)));
		rules.add(rule(b -> b.expression(and(b.a(StandardLicenseTerm.Notice), or(b.b(StandardUsageTerm.DistributionPublic), and(b.b(StandardUsageTerm.DistributionService), b.a(StandardLicenseTerm.SaaSIsDistribution))))).finding(CopyrightNoticeFinding::new)));
		// SaaSIsDistribution has no satisfying condition, since it's part of the distribution usage terms
		// SameLicense is handled elsewhere, since it speaks to the licenses across artifacts and the analyzer isn't graph-aware
		rules.add(rule(b -> b.expression(and(or(b.b(StandardUsageTerm.DistributionPublic), and(b.b(StandardUsageTerm.DistributionService), b.a(StandardLicenseTerm.SaaSIsDistribution))), b.b(StandardUsageTerm.UseModified), b.a(StandardLicenseTerm.StateChanges))).finding(StateChangesFinding::new)));

		// License limitations
		rules.add(rule(b -> b.expression(implies(b.a(StandardLicenseTerm.NoRedistribution), not(and(b.b(StandardUsageTerm.DistributionPublic), b.b(StandardUsageTerm.DistributingSource))))).finding(ConditionFinding::new)));

		// Consistency rules
		rules.add(rule(b -> b.expression(not(or(b.b(StandardUsageTerm.DistributionPublic), b.b(StandardUsageTerm.DistributionPrivate), b.b(StandardUsageTerm.DistributionService)))).finding(StandardUsageTermAttribute.Distribution)));
		rules.add(rule(b -> b.expression(not(or(b.b(StandardUsageTerm.UseLink), b.b(StandardUsageTerm.UseCopy), b.b(StandardUsageTerm.UseModified)))).finding(StandardUsageTermAttribute.Consumption)));
		rules.add(rule(b -> b.expression(not(or(b.b(StandardUsageTerm.DistributingBinary), b.b(StandardUsageTerm.DistributingSource)))).finding(StandardUsageTermAttribute.Format)));

		// Ignored terms
		rules.add(rule(b -> b.expression(b.a(StandardLicenseTerm.SameLicense))));
		rules.add(rule(b -> b.expression(b.a(StandardLicenseTerm.SaaSIsDistribution))));

		return rules;
	}

	@Override
	protected IContractComparisonScheme<ILicenseTerm, IUsageTerm> getScheme() {
		return LicenseUsageNameScheme.create();
	}
}