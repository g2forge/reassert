package com.g2forge.reassert.term;

import static com.g2forge.reassert.term.analyze.model.logic.HTermLogic.*;

import java.util.ArrayList;
import java.util.Collection;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.term.analyze.model.findings.ConditionFinding;
import com.g2forge.reassert.term.analyze.model.findings.DiscloseSourceFinding;
import com.g2forge.reassert.term.analyze.model.findings.NoticeFinding;
import com.g2forge.reassert.term.analyze.model.findings.StateChangesFinding;
import com.g2forge.reassert.term.analyze.model.findings.SuspiciousUsageFinding;
import com.g2forge.reassert.term.analyze.model.rules.IRules;
import com.g2forge.reassert.term.analyze.model.rules.Rule;

import lombok.Getter;

public class StandardLicenseUsageRules implements IRules, ISingleton {
	protected static final StandardLicenseUsageRules INSTANCE = new StandardLicenseUsageRules();

	public static StandardLicenseUsageRules create() {
		return INSTANCE;
	}

	@Getter
	protected final Collection<Rule> rules;

	protected StandardLicenseUsageRules() {
		rules = new ArrayList<>();

		// Usage terms
		rules.add(Rule.builder().satisfied(StandardUsageTerm.Commercial).expression$(or(not(StandardUsageTerm.Commercial), StandardLicenseTerm.CommercialUse)).finding(ConditionFinding::new).build());

		rules.add(Rule.builder().satisfied(StandardUsageTerm.DistributionPublic).expression$(or(not(StandardUsageTerm.DistributionPublic), StandardLicenseTerm.Distribution)).finding(ConditionFinding::new).build());
		rules.add(Rule.builder().satisfied(StandardUsageTerm.DistributionPrivate).expression$(or(not(StandardUsageTerm.DistributionPrivate), StandardLicenseTerm.PrivateUse)).finding(ConditionFinding::new).build());
		rules.add(Rule.builder().satisfied(StandardUsageTerm.DistributionService).expression$(or(not(StandardUsageTerm.DistributionService), not(StandardLicenseTerm.SaaSIsDistribution), StandardLicenseTerm.Distribution)).finding(ConditionFinding::new).build());

		rules.add(Rule.builder().satisfied(StandardUsageTerm.UseLink).expression$(or(not(StandardUsageTerm.UseLink), StandardLicenseTerm.PrivateUse)).finding(ConditionFinding::new).build());
		rules.add(Rule.builder().satisfied(StandardUsageTerm.UseCopy).expression$(or(not(StandardUsageTerm.UseCopy), StandardLicenseTerm.PrivateUse)).finding(ConditionFinding::new).build());
		rules.add(Rule.builder().satisfied(StandardUsageTerm.UseModified).expression$(or(not(StandardUsageTerm.UseModified), and(StandardLicenseTerm.PrivateUse, StandardLicenseTerm.Modification))).finding(ConditionFinding::new).build());

		rules.add(Rule.builder().satisfied(StandardUsageTerm.DistributingBinary).expression$(or(not(StandardUsageTerm.DistributingBinary), StandardLicenseTerm.PrivateUse)).finding(ConditionFinding::new).build());
		rules.add(Rule.builder().satisfied(StandardUsageTerm.DistributingSource).expression$(or(not(StandardUsageTerm.DistributingSource), StandardLicenseTerm.PrivateUse)).finding(ConditionFinding::new).build());

		// License conditions
		rules.add(Rule.builder().satisfied(StandardLicenseTerm.DisclosureSource).expression$(and(or(StandardUsageTerm.DistributionPublic, and(StandardUsageTerm.DistributionService, StandardLicenseTerm.SaaSIsDistribution)), StandardLicenseTerm.DisclosureSource)).finding(DiscloseSourceFinding::new).build());
		rules.add(Rule.builder().satisfied(StandardLicenseTerm.Notice).expression$(and(StandardLicenseTerm.Notice, or(StandardUsageTerm.DistributionPublic, and(StandardUsageTerm.DistributionService, StandardLicenseTerm.SaaSIsDistribution)))).finding(NoticeFinding::new).build());
		// SaaSIsDistribution has no satisfying condition, since it's part of the distribution usage terms
		// SameLicense is handled elsewhere, since it speaks to the licenses across artifacts and the analyzer isn't graph-aware
		rules.add(Rule.builder().satisfied(StandardLicenseTerm.StateChanges).expression$(and(or(StandardUsageTerm.DistributionPublic, and(StandardUsageTerm.DistributionService, StandardLicenseTerm.SaaSIsDistribution)), StandardUsageTerm.UseModified, StandardLicenseTerm.StateChanges)).finding(StateChangesFinding::new).build());

		// Consistency rules
		rules.add(Rule.builder().expression$(not(or(StandardUsageTerm.DistributionPublic, StandardUsageTerm.DistributionPrivate, StandardUsageTerm.DistributionService))).finding(t -> new SuspiciousUsageFinding(t, "method of distribtion")).build());
		rules.add(Rule.builder().expression$(not(or(StandardUsageTerm.UseLink, StandardUsageTerm.UseCopy, StandardUsageTerm.UseModified))).finding(t -> new SuspiciousUsageFinding(t, "method of consumption")).build());
		rules.add(Rule.builder().expression$(not(or(StandardUsageTerm.DistributingBinary, StandardUsageTerm.DistributingSource))).finding(t -> new SuspiciousUsageFinding(t, "format")).build());

		// Ignored terms
		rules.add(Rule.builder().satisfied(StandardLicenseTerm.SameLicense).build());
		rules.add(Rule.builder().satisfied(StandardLicenseTerm.SaaSIsDistribution).build());
	}
}