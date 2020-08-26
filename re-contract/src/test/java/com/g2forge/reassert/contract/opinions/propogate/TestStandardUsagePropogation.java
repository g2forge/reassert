package com.g2forge.reassert.contract.opinions.propogate;

import org.junit.Test;

import com.g2forge.alexandria.test.HAssert;
import com.g2forge.reassert.contract.opinions.propogate.StandardUsagePropogation;
import com.g2forge.reassert.contract.usage.StandardUsageTerm;
import com.g2forge.reassert.core.model.artifact.Inherits;
import com.g2forge.reassert.core.model.contract.Terms;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.core.model.contract.usage.Usage;

public class TestStandardUsagePropogation {
	@Test
	public void inherits() {
		final Usage input = new Usage("Usage", Terms.<IUsageTerm>builder().include(StandardUsageTerm.Commercial).build());
		final Usage expected = new Usage("Inherits() Usage", Terms.<IUsageTerm>builder().include(StandardUsageTerm.Commercial, StandardUsageTerm.UseLink).exclude(StandardUsageTerm.UseCopy, StandardUsageTerm.UseModified).unspecified(StandardUsageTerm.DistributionPublic, StandardUsageTerm.DistributionPrivate, StandardUsageTerm.DistributionService, StandardUsageTerm.DistributingBinary, StandardUsageTerm.DistributingSource).build());
		HAssert.assertEquals(expected, StandardUsagePropogation.create().apply(new Inherits(), input));
	}
}