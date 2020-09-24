package com.g2forge.reassert.standard.algorithm;

import static com.g2forge.reassert.express.model.operation.BooleanOperation.and;
import static com.g2forge.reassert.express.model.operation.BooleanOperation.not;
import static com.g2forge.reassert.express.model.operation.BooleanOperation.or;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.finding.ConditionFinding;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.finding.CopyrightNoticeFinding;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.finding.DiscloseSourceFinding;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.finding.StateChangesFinding;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.rule.ALicenseUsageRules;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.rule.ILicenseUsageRule;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.standard.model.contract.license.StandardLicenseTerm;
import com.g2forge.reassert.standard.model.contract.usage.StandardUsageTerm;
import com.g2forge.reassert.standard.model.contract.usage.StandardUsageTermAttribute;

import lombok.Getter;

@ReassertLegalOpinion
public class StandardLicenseUsageRules extends ALicenseUsageRules implements ISingleton {
	protected static final StandardLicenseUsageRules INSTANCE = new StandardLicenseUsageRules();

	public static StandardLicenseUsageRules create() {
		return INSTANCE;
	}

	@Getter(lazy = true)
	private final List<ILicenseUsageRule> rules = Collections.unmodifiableList(computeRules());

	protected StandardLicenseUsageRules() {}

	protected List<ILicenseUsageRule> computeRules() {
		final List<ILicenseUsageRule> rules = new ArrayList<>();

		// Usage terms
		rules.add(rule(b -> b.expression(or(b.not(StandardUsageTerm.Commercial), b.of(StandardLicenseTerm.CommercialUse))).finding(ConditionFinding::new)));

		rules.add(rule(b -> b.expression(or(b.not(StandardUsageTerm.DistributionPublic), b.of(StandardLicenseTerm.Distribution))).finding(ConditionFinding::new)));
		rules.add(rule(b -> b.expression(or(b.not(StandardUsageTerm.DistributionPrivate), b.of(StandardLicenseTerm.PrivateUse))).finding(ConditionFinding::new)));
		rules.add(rule(b -> b.expression(or(b.not(StandardUsageTerm.DistributionService), b.not(StandardLicenseTerm.SaaSIsDistribution), b.of(StandardLicenseTerm.Distribution))).finding(ConditionFinding::new)));

		rules.add(rule(b -> b.expression(or(b.not(StandardUsageTerm.UseLink), b.of(StandardLicenseTerm.PrivateUse))).finding(ConditionFinding::new)));
		rules.add(rule(b -> b.expression(or(b.not(StandardUsageTerm.UseCopy), b.of(StandardLicenseTerm.PrivateUse))).finding(ConditionFinding::new)));
		rules.add(rule(b -> b.expression(or(b.not(StandardUsageTerm.UseModified), and(b.of(StandardLicenseTerm.PrivateUse), b.of(StandardLicenseTerm.Modification)))).finding(ConditionFinding::new)));

		rules.add(rule(b -> b.expression(or(b.not(StandardUsageTerm.DistributingBinary), b.of(StandardLicenseTerm.PrivateUse))).finding(ConditionFinding::new)));
		rules.add(rule(b -> b.expression(or(b.not(StandardUsageTerm.DistributingSource), b.of(StandardLicenseTerm.PrivateUse))).finding(ConditionFinding::new)));

		// License conditions
		rules.add(rule(b -> b.expression(and(or(b.of(StandardUsageTerm.DistributionPublic), and(b.of(StandardUsageTerm.DistributionService), b.of(StandardLicenseTerm.SaaSIsDistribution))), b.of(StandardLicenseTerm.DisclosureSource))).finding(DiscloseSourceFinding::new)));
		rules.add(rule(b -> b.expression(and(b.of(StandardLicenseTerm.Notice), or(b.of(StandardUsageTerm.DistributionPublic), and(b.of(StandardUsageTerm.DistributionService), b.of(StandardLicenseTerm.SaaSIsDistribution))))).finding(CopyrightNoticeFinding::new)));
		// SaaSIsDistribution has no satisfying condition, since it's part of the distribution usage terms
		// SameLicense is handled elsewhere, since it speaks to the licenses across artifacts and the analyzer isn't graph-aware
		rules.add(rule(b -> b.expression(and(or(b.of(StandardUsageTerm.DistributionPublic), and(b.of(StandardUsageTerm.DistributionService), b.of(StandardLicenseTerm.SaaSIsDistribution))), b.of(StandardUsageTerm.UseModified), b.of(StandardLicenseTerm.StateChanges))).finding(StateChangesFinding::new)));

		// Consistency rules
		rules.add(rule(b -> b.expression(not(or(b.of(StandardUsageTerm.DistributionPublic), b.of(StandardUsageTerm.DistributionPrivate), b.of(StandardUsageTerm.DistributionService)))).finding(StandardUsageTermAttribute.Distribution)));
		rules.add(rule(b -> b.expression(not(or(b.of(StandardUsageTerm.UseLink), b.of(StandardUsageTerm.UseCopy), b.of(StandardUsageTerm.UseModified)))).finding(StandardUsageTermAttribute.Consumption)));
		rules.add(rule(b -> b.expression(not(or(b.of(StandardUsageTerm.DistributingBinary), b.of(StandardUsageTerm.DistributingSource)))).finding(StandardUsageTermAttribute.Format)));

		// Ignored terms
		rules.add(rule(b -> b.expression(b.of(StandardLicenseTerm.SameLicense))));
		rules.add(rule(b -> b.expression(b.of(StandardLicenseTerm.SaaSIsDistribution))));

		return rules;
	}
}