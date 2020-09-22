package com.g2forge.reassert.standard.v2.algorithm;

import static com.g2forge.reassert.contract.v2.model.TermOperation.and;
import static com.g2forge.reassert.contract.v2.model.TermOperation.not;
import static com.g2forge.reassert.contract.v2.model.TermOperation.or;

import com.g2forge.reassert.contract.v2.model.CTNameType;
import com.g2forge.reassert.contract.v2.model.ICTName;
import com.g2forge.reassert.contract.v2.model.finding.rule.ConditionFinding;
import com.g2forge.reassert.contract.v2.model.finding.rule.DiscloseSourceFinding;
import com.g2forge.reassert.contract.v2.model.finding.rule.NoticeFinding;
import com.g2forge.reassert.contract.v2.model.finding.rule.StateChangesFinding;
import com.g2forge.reassert.contract.v2.model.rule.IRules;
import com.g2forge.reassert.contract.v2.model.rule.Rule;
import com.g2forge.reassert.contract.v2.model.rule.Rules;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.contract.terms.ITerm;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.v2.model.variable.Variable;
import com.g2forge.reassert.standard.model.contract.license.StandardLicenseTerm;
import com.g2forge.reassert.standard.model.contract.usage.StandardUsageTerm;
import com.g2forge.reassert.standard.model.contract.usage.StandardUsageTermAttribute2;

@ReassertLegalOpinion
public class StandardLicenseUsageRules {
	protected static final IRules INSTANCE = computeRules();

	public static IRules create() {
		return INSTANCE;
	}

	protected static IRules computeRules() {
		final Rules.RulesBuilder rules = Rules.builder();

		// Usage terms
		rules.rule(Rule.builder().expression(or(not(StandardUsageTerm.Commercial), of(StandardLicenseTerm.CommercialUse))).finding(ConditionFinding::new).build());

		rules.rule(Rule.builder().expression(or(not(StandardUsageTerm.DistributionPublic), of(StandardLicenseTerm.Distribution))).finding(ConditionFinding::new).build());
		rules.rule(Rule.builder().expression(or(not(StandardUsageTerm.DistributionPrivate), of(StandardLicenseTerm.PrivateUse))).finding(ConditionFinding::new).build());
		rules.rule(Rule.builder().expression(or(not(StandardUsageTerm.DistributionService), not(StandardLicenseTerm.SaaSIsDistribution), of(StandardLicenseTerm.Distribution))).finding(ConditionFinding::new).build());

		rules.rule(Rule.builder().expression(or(not(StandardUsageTerm.UseLink), of(StandardLicenseTerm.PrivateUse))).finding(ConditionFinding::new).build());
		rules.rule(Rule.builder().expression(or(not(StandardUsageTerm.UseCopy), of(StandardLicenseTerm.PrivateUse))).finding(ConditionFinding::new).build());
		rules.rule(Rule.builder().expression(or(not(StandardUsageTerm.UseModified), and(of(StandardLicenseTerm.PrivateUse), of(StandardLicenseTerm.Modification)))).finding(ConditionFinding::new).build());

		rules.rule(Rule.builder().expression(or(not(StandardUsageTerm.DistributingBinary), of(StandardLicenseTerm.PrivateUse))).finding(ConditionFinding::new).build());
		rules.rule(Rule.builder().expression(or(not(StandardUsageTerm.DistributingSource), of(StandardLicenseTerm.PrivateUse))).finding(ConditionFinding::new).build());

		// License conditions
		rules.rule(Rule.builder().expression(and(or(of(StandardUsageTerm.DistributionPublic), and(of(StandardUsageTerm.DistributionService), of(StandardLicenseTerm.SaaSIsDistribution))), of(StandardLicenseTerm.DisclosureSource))).finding(DiscloseSourceFinding::new).build());
		rules.rule(Rule.builder().expression(and(of(StandardLicenseTerm.Notice), or(of(StandardUsageTerm.DistributionPublic), and(of(StandardUsageTerm.DistributionService), of(StandardLicenseTerm.SaaSIsDistribution))))).finding(NoticeFinding::new).build());
		// SaaSIsDistribution has no satisfying condition, since it's part of the distribution usage terms
		// SameLicense is handled elsewhere, since it speaks to the licenses across artifacts and the analyzer isn't graph-aware
		rules.rule(Rule.builder().expression(and(or(of(StandardUsageTerm.DistributionPublic), and(of(StandardUsageTerm.DistributionService), of(StandardLicenseTerm.SaaSIsDistribution))), of(StandardUsageTerm.UseModified), of(StandardLicenseTerm.StateChanges))).finding(StateChangesFinding::new).build());

		// Consistency rules
		rules.rule(Rule.builder().expression(not(or(of(StandardUsageTerm.DistributionPublic), of(StandardUsageTerm.DistributionPrivate), of(StandardUsageTerm.DistributionService)))).finding(StandardUsageTermAttribute2.Distribution).build());
		rules.rule(Rule.builder().expression(not(or(of(StandardUsageTerm.UseLink), of(StandardUsageTerm.UseCopy), of(StandardUsageTerm.UseModified)))).finding(StandardUsageTermAttribute2.Consumption).build());
		rules.rule(Rule.builder().expression(not(or(of(StandardUsageTerm.DistributingBinary), of(StandardUsageTerm.DistributingSource)))).finding(StandardUsageTermAttribute2.Format).build());

		// Ignored terms
		rules.rule(Rule.builder().expression(of(StandardLicenseTerm.SameLicense)).build());
		rules.rule(Rule.builder().expression(of(StandardLicenseTerm.SaaSIsDistribution)).build());

		return rules.build();
	}

	protected static Variable<ICTName, TermRelation> of(final ITerm term) {
		return new Variable<>(new CTNameType(term));
	}
}