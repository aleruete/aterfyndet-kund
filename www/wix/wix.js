
import { currentMember } from 'wix-members';

$w.onReady(function () {
    currentMember.getMember()
        .then((member) => {
            if (member) {
                let memberId = member._id; // Get the logged-in member's ID
                console.log("Member ID:", memberId); // Print to console

                $w("#html1").postMessage({ memberId: memberId }); // Send ID to iframe
            } else {
                console.log("No member is logged in.");
            }
        })
        .catch((error) => {
            console.error("Error fetching member:", error);
        });
});