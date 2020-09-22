package com.g2forge.reassert.standard.v2.algorithm;

import static com.g2forge.reassert.contract.v2.model.TermOperation.not;
import static com.g2forge.reassert.contract.v2.model.TermOperation.or;

import com.g2forge.reassert.contract.v2.model.CTName;
import com.g2forge.reassert.contract.v2.model.ICTName;
import com.g2forge.reassert.contract.v2.model.finding.rule.ConditionFinding;
import com.g2forge.reassert.contract.v2.model.rule.IRules;
import com.g2forge.reassert.contract.v2.model.rule.Rule;
import com.g2forge.reassert.contract.v2.model.rule.Rules;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.contract.terms.ITerm;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.v2.model.variable.Variable;
import com.g2forge.reassert.standard.model.contract.license.StandardLicenseTerm;
import com.g2forge.reassert.standard.model.contract.usage.StandardUsageTerm;

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

		return rules.build();
	}

	protected static Variable<ICTName, TermRelation> of(final ITerm term) {
		return new Variable<>(new CTName(term));
	}
}