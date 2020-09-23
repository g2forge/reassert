package com.g2forge.reassert.standard.v2.algorithm;

import static com.g2forge.reassert.contract.v2.model.licenseusage.CTOperation.and;
import static com.g2forge.reassert.contract.v2.model.licenseusage.CTOperation.not;
import static com.g2forge.reassert.contract.v2.model.licenseusage.CTOperation.of;
import static com.g2forge.reassert.contract.v2.model.licenseusage.CTOperation.or;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.contract.v2.algorithm.ALicenseUsageRules;
import com.g2forge.reassert.contract.v2.model.finding.rule.ConditionFinding;
import com.g2forge.reassert.contract.v2.model.finding.rule.DiscloseSourceFinding;
import com.g2forge.reassert.contract.v2.model.finding.rule.NoticeFinding;
import com.g2forge.reassert.contract.v2.model.finding.rule.StateChangesFinding;
import com.g2forge.reassert.contract.v2.model.licenseusage.rule.IRule;
import com.g2forge.reassert.contract.v2.model.licenseusage.rule.Rule;
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
	private final List<IRule> rules = Collections.unmodifiableList(computeRules());

	protected StandardLicenseUsageRules() {}

	protected List<IRule> computeRules() {
		final List<IRule> rules = new ArrayList<>();

		// Usage terms
		rules.add(Rule.builder().expression(or(not(StandardUsageTerm.Commercial), of(StandardLicenseTerm.CommercialUse))).finding(ConditionFinding::new).build());

		rules.add(Rule.builder().expression(or(not(StandardUsageTerm.DistributionPublic), of(StandardLicenseTerm.Distribution))).finding(ConditionFinding::new).build());
		rules.add(Rule.builder().expression(or(not(StandardUsageTerm.DistributionPrivate), of(StandardLicenseTerm.PrivateUse))).finding(ConditionFinding::new).build());
		rules.add(Rule.builder().expression(or(not(StandardUsageTerm.DistributionService), not(StandardLicenseTerm.SaaSIsDistribution), of(StandardLicenseTerm.Distribution))).finding(ConditionFinding::new).build());

		rules.add(Rule.builder().expression(or(not(StandardUsageTerm.UseLink), of(StandardLicenseTerm.PrivateUse))).finding(ConditionFinding::new).build());
		rules.add(Rule.builder().expression(or(not(StandardUsageTerm.UseCopy), of(StandardLicenseTerm.PrivateUse))).finding(ConditionFinding::new).build());
		rules.add(Rule.builder().expression(or(not(StandardUsageTerm.UseModified), and(of(StandardLicenseTerm.PrivateUse), of(StandardLicenseTerm.Modification)))).finding(ConditionFinding::new).build());

		rules.add(Rule.builder().expression(or(not(StandardUsageTerm.DistributingBinary), of(StandardLicenseTerm.PrivateUse))).finding(ConditionFinding::new).build());
		rules.add(Rule.builder().expression(or(not(StandardUsageTerm.DistributingSource), of(StandardLicenseTerm.PrivateUse))).finding(ConditionFinding::new).build());

		// License conditions
		rules.add(Rule.builder().expression(and(or(of(StandardUsageTerm.DistributionPublic), and(of(StandardUsageTerm.DistributionService), of(StandardLicenseTerm.SaaSIsDistribution))), of(StandardLicenseTerm.DisclosureSource))).finding(DiscloseSourceFinding::new).build());
		rules.add(Rule.builder().expression(and(of(StandardLicenseTerm.Notice), or(of(StandardUsageTerm.DistributionPublic), and(of(StandardUsageTerm.DistributionService), of(StandardLicenseTerm.SaaSIsDistribution))))).finding(NoticeFinding::new).build());
		// SaaSIsDistribution has no satisfying condition, since it's part of the distribution usage terms
		// SameLicense is handled elsewhere, since it speaks to the licenses across artifacts and the analyzer isn't graph-aware
		rules.add(Rule.builder().expression(and(or(of(StandardUsageTerm.DistributionPublic), and(of(StandardUsageTerm.DistributionService), of(StandardLicenseTerm.SaaSIsDistribution))), of(StandardUsageTerm.UseModified), of(StandardLicenseTerm.StateChanges))).finding(StateChangesFinding::new).build());

		// Consistency rules
		rules.add(Rule.builder().expression(not(or(of(StandardUsageTerm.DistributionPublic), of(StandardUsageTerm.DistributionPrivate), of(StandardUsageTerm.DistributionService)))).finding(StandardUsageTermAttribute.Distribution).build());
		rules.add(Rule.builder().expression(not(or(of(StandardUsageTerm.UseLink), of(StandardUsageTerm.UseCopy), of(StandardUsageTerm.UseModified)))).finding(StandardUsageTermAttribute.Consumption).build());
		rules.add(Rule.builder().expression(not(or(of(StandardUsageTerm.DistributingBinary), of(StandardUsageTerm.DistributingSource)))).finding(StandardUsageTermAttribute.Format).build());

		// Ignored terms
		rules.add(Rule.builder().expression(of(StandardLicenseTerm.SameLicense)).build());
		rules.add(Rule.builder().expression(of(StandardLicenseTerm.SaaSIsDistribution)).build());

		return rules;
	}
}